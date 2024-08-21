#![allow(dead_code)]

use std::rc::Rc;

use super::source::SourceRef;

#[derive(Debug, Clone)]
pub enum LexError {
    InvalidCharacter(Rc<SourceRef>),
    CannotMakeSignedNumber(Rc<SourceRef>),
    CannotMakeUnsignedNumber(Rc<SourceRef>),
    EmptyCharacterLiteral(Rc<SourceRef>),
    UnterminatedCharacterLiteral(Rc<SourceRef>),
    UnterminatedStringLiteral(Rc<SourceRef>),
    DecimalLiteralWithMultipleDecimalPoints(Rc<SourceRef>),
}

#[derive(Debug, Clone)]
pub enum ParseError {
    Expected(String, Rc<SourceRef>, Option<String>),
    ConstantDeclarationNeedsTypeOrInitValue(Rc<SourceRef>),
    CannotParseAnExpression(Rc<SourceRef>),
    MalformedDeclaration(String, Rc<SourceRef>),
    NoVariableAtCurrentScope(Rc<SourceRef>),
    NoCodeBlockAllowedInCurrentContext(Rc<SourceRef>),
    NoLoopAtTopLevel(Rc<SourceRef>),
    NoBreakOutsideLoop(Rc<SourceRef>),
    NoContinueOutsideLoop(Rc<SourceRef>),
    MisuseOfPubKeyword(Rc<SourceRef>),
    UnterminatedCodeBlock(Rc<SourceRef>, Option<String>),
    ReturnInstructionOutsideFunction(Rc<SourceRef>),
    CyclicalDependencyBetweenNodes { cycle: String, src: Rc<SourceRef> },
    ParsedInstructionIsNotAllowedAtThisLevel { level: String, src: Rc<SourceRef> },
    TooManyErrors(Rc<SourceRef>),
}

impl ParseError {
    pub fn get_error_src(&self) -> Rc<SourceRef> {
        match self {
            ParseError::Expected(_, src, _)
            | ParseError::ConstantDeclarationNeedsTypeOrInitValue(src)
            | ParseError::CannotParseAnExpression(src)
            | ParseError::MalformedDeclaration(_, src)
            | ParseError::NoCodeBlockAllowedInCurrentContext(src)
            | ParseError::NoLoopAtTopLevel(src)
            | ParseError::NoBreakOutsideLoop(src)
            | ParseError::NoContinueOutsideLoop(src)
            | ParseError::MisuseOfPubKeyword(src)
            | ParseError::UnterminatedCodeBlock(src, _)
            | ParseError::ReturnInstructionOutsideFunction(src)
            | ParseError::TooManyErrors(src)
            | ParseError::CyclicalDependencyBetweenNodes { src, .. }
            | ParseError::ParsedInstructionIsNotAllowedAtThisLevel { src, .. }
            | ParseError::NoVariableAtCurrentScope(src) => src.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CheckerError {
    TypeMismatch {
        loc: Rc<SourceRef>,
        expected: String,
        found: String,
    },
    NumberTypeDefaultInferenceFailed {
        loc: Rc<SourceRef>,
        number: String,
    },
    DecimalTypeDefaultInferenceFailed {
        loc: Rc<SourceRef>,
        number: String,
    },
    NumberTypeInferenceFailed {
        loc: Rc<SourceRef>,
        number: String,
        given_type: String,
    },
    DecimalTypeInferenceFailed {
        loc: Rc<SourceRef>,
        number: String,
        given_type: String,
    },
    ReferenceToUndefinedName {
        loc: Rc<SourceRef>,
        var_name: String,
    },
    InvalidUseOfBinaryOperator {
        loc: Rc<SourceRef>,
        op: String,
        left: String,
        right: String,
    },
    InvalidUseOfUnaryOperator {
        loc: Rc<SourceRef>,
        op: String,
        operand: String,
        tip: Option<String>,
    },
    InvalidType {
        loc: Rc<SourceRef>,
        type_name: String,
    },
    IncompleteType {
        loc: Rc<SourceRef>,
        type_name: String,
    },
    // TODO: track the location of the previous definition
    NameAlreadyDefined {
        loc: Rc<SourceRef>,
        name: String,
    },
    UseOfUninitializedVariable {
        loc: Rc<SourceRef>,
        name: String,
    },
    UseOfErroredVariableOrConstant {
        is_const: bool,
        loc: Rc<SourceRef>,
        name: String,
    },
    NameIsNotCallable {
        name: String,
        name_ty: String,
        loc: Rc<SourceRef>,
    },
    MismatchingReturnType {
        exp: String,
        given: String,
        loc_given: Rc<SourceRef>,
    },
    IncorrectFunctionArity {
        func: String,
        exp: usize,
        given: usize,
        loc_given: Rc<SourceRef>,
    },
    CannotInferTypeOfEmptyArray {
        loc: Rc<SourceRef>,
    },
    CannotInferTypeOfEmptyHashMap {
        loc: Rc<SourceRef>,
    },
    MismatchingStaticArrayItemTypes {
        expected_ty: String,
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    StaticArrayTypeCheckFailed {
        given_ty: String,
        arr_loc: Rc<SourceRef>,
    },
    TupleTypeCheckFailed {
        given_ty: String,
        tup_loc: Rc<SourceRef>,
    },
    OptionalTypeInferenceFailed {
        given_ty: String,
        opt_loc: Rc<SourceRef>,
    },
    OptionalTypeInferenceFailedWithoutContextualTy {
        opt_loc: Rc<SourceRef>,
    },
    NonConstantNumberSizeForStaticArray {
        loc: Rc<SourceRef>,
    },
    MismismatchStaticArrayLength {
        exp: String,
        given: String,
        arr_loc: Rc<SourceRef>,
    },
    ConditionShouldBeTypedBool {
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    ExpectedArrayOrSlice {
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    PrintRequiresAStringArg {
        is_println: bool,
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    IndexIntoOpRequiresArraySliceOrString {
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    Expected(String, Rc<SourceRef>, Option<String>),
    AccessMemberOpCannotBePerformedOnType {
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    MemberDoesNotExist {
        given_ty: String,
        mem: String,
        loc: Rc<SourceRef>,
    },
    StructHasNoInitFunction {
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    CannotAssignToTarget {
        target: String,
        loc: Rc<SourceRef>,
    },
    CannotAssignToImmutableTarget {
        target: String,
        loc: Rc<SourceRef>,
    },
    CannotAccessNonConstFuncOnConstTarget {
        loc: Rc<SourceRef>,
    },
    CannotReturnFromInsideADeferIns {
        loc: Rc<SourceRef>,
    },
    FunctionInDeferShouldReturnVoid {
        loc: Rc<SourceRef>,
    },
    LoopControlInstructionOutsideLoop {
        ty: String,
        loc: Rc<SourceRef>,
    },
    InvalidForInLoopTargetType {
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    CannotDerefNonPtrType {
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    CannotTakeAddressOfExpr {
        loc: Rc<SourceRef>,
    },
    CannotFreeNonPtrType {
        given_ty: String,
        loc: Rc<SourceRef>,
    },
    HashMapTypeCheckFailed {
        given_ty: String,
        arr_loc: Rc<SourceRef>,
    },
    TooManyErrors,
}

#[derive(Debug, Clone)]
pub enum CodeGenError {
    CompileErr(),
}
