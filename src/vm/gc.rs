#![allow(dead_code)]

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

/// These are values that live on the stack
#[derive(Debug, Clone)]
enum Value {
    Object(Weak<Object>),
}

/// Data that lives inside of an object, like heaped values
#[derive(Debug, Clone)]
enum ObjectData {
    Str(String),
}

/// Objects are values that live in the heap
#[derive(Debug)]
struct Object {
    // Mutable state within the object
    data: RefCell<ObjectData>,
    marked: Cell<bool>,
}

/// Generation contains objects
/// There is nothing unique in a generation itself,
/// it is just used as a separate pool for objects
struct Generation {
    objects: Vec<Rc<Object>>,
}

impl Generation {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
        }
    }

    pub fn sweep(&mut self) {
        self.objects.retain(|obj| {
            let is_marked = obj.marked.get();
            if cfg!(Debug) && !is_marked {
                // Object is unreachable, "deallocate" it
                // NOTE: Obviously as an RC, it will dealloc once the Rc is dropped
                println!(
                    "Deallocating object with data: {:?} {}",
                    obj.data,
                    Rc::strong_count(&obj),
                );
            }

            // Reset mark
            obj.marked.set(false);
            is_marked
        });
    }

    pub fn transfer(&mut self, from: &mut Self) {
        if cfg!(Debug) {
            for item in &from.objects {
                println!("Moving to next generation {:?}", item.data);
            }
        }
        self.objects.append(&mut from.objects);
        from.objects.clear();
    }
}

struct GarbageCollector {
    young: Generation,
    old: Generation,

    bytes_allocated: usize,
    next_sweep: usize,
    generation_counter: u8,
}

/// Total initial bytes before a collection occurs
const INITIAL_COLLECTION_SIZE: usize = 1024 * 1024;
/// Threshold multiplier applied to next_sweep size
const SWEEP_FACTOR: usize = 2;
/// Amount of collections before managing the old generation
const GENERATION_SWEEP: u8 = 3;

/// An object that can be threaded through the garbage collector,
/// as multiple parameters would be gross
struct Roots<'a> {
    stack: &'a [Value],
    interned_strings: &'a HashMap<u32, Rc<Object>>,
}

impl GarbageCollector {
    fn new() -> GarbageCollector {
        GarbageCollector {
            young: Generation::new(),
            old: Generation::new(),
            bytes_allocated: 0,
            next_sweep: INITIAL_COLLECTION_SIZE,
            generation_counter: 0,
        }
    }

    fn allocate<'a>(&mut self, data: ObjectData, roots: Roots<'a>) -> Weak<Object> {
        self.bytes_allocated += std::mem::size_of_val(&data);

        // Check for next collection
        if self.bytes_allocated >= self.next_sweep {
            self.collect_garbage(roots);
        }

        let obj = Rc::new(Object {
            data: RefCell::new(data),
            marked: Cell::new(false),
        });
        let weak = Rc::downgrade(&obj);
        self.young.objects.push(obj);
        weak
    }

    fn mark(&self, root: &Rc<Object>) {
        if root.marked.get() {
            return;
        }

        root.marked.set(true);
    }

    fn sweep(&mut self) {
        self.young.sweep();

        self.generation_counter += 1;
        if self.generation_counter >= GENERATION_SWEEP {
            self.old.sweep();
            self.generation_counter = 0;
        }

        // Append young to old
        self.old.transfer(&mut self.young);

        // Set next sweep point
        self.next_sweep *= SWEEP_FACTOR;
    }

    #[allow(irrefutable_let_patterns)]
    fn mark_roots<'a>(&mut self, roots: Roots<'a>) {
        for item in roots.stack {
            if let Value::Object(obj) = item {
                if let Some(object) = obj.upgrade() {
                    self.mark(&object);
                }
            }
        }

        for item in roots.interned_strings.values() {
            self.mark(item);
        }
    }

    fn collect_garbage<'a>(&mut self, roots: Roots<'a>) {
        self.mark_roots(roots);
        self.sweep();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct VM {
        stack: Vec<Value>,
        gc: GarbageCollector,
        interned_strings: HashMap<u32, Rc<Object>>,
    }

    impl VM {
        fn new() -> Self {
            Self {
                stack: Vec::with_capacity(8),
                gc: GarbageCollector::new(),
                interned_strings: HashMap::new(),
            }
        }

        fn push(&mut self, item: Value) {
            self.stack.push(item);
        }

        fn pop(&mut self) {
            assert!(self.stack.len() > 0);
            _ = self.stack.pop();
        }

        fn allocate(&mut self, data: ObjectData) -> Weak<Object> {
            self.gc.allocate(
                data,
                Roots {
                    stack: &self.stack,
                    interned_strings: &self.interned_strings,
                },
            )
        }

        fn collect(&mut self) {
            self.gc.collect_garbage(Roots {
                stack: &self.stack,
                interned_strings: &self.interned_strings,
            })
        }

        fn collect_all(&mut self) {
            self.gc.young.sweep();
            self.gc.old.sweep();
        }
    }

    #[test]
    fn simple_collection() {
        let mut vm = VM::new();

        let value = Value::Object(vm.allocate(ObjectData::Str("Goodbye".into())));
        vm.push(value);

        {
            // Allocate objects
            let obj2 = vm.allocate(ObjectData::Str("Hello".into()));

            obj2.upgrade().as_mut().map(|obj2| {
                // Modify mutable state within objects
                *obj2.data.borrow_mut() = ObjectData::Str("Hello, World!".into());
            });

            // Obj2 is not on in the roots, so it is not reachable by the GC
            vm.collect();
        }

        assert_eq!(vm.gc.young.objects.len(), 0);
        assert_eq!(vm.gc.old.objects.len(), 1);

        vm.pop();
        vm.collect_all();

        assert_eq!(vm.gc.old.objects.len(), 0);
    }
}
