from time import monotonic
from textual.app import App, ComposeResult
from textual.widgets import Header, Footer, Button, Static
from textual.containers import ScrollableContainer
from textual.reactive import reactive

class TimeDisplay(Static):
    """A simple time display."""

    start_time = reactive(monotonic)
    time = reactive(0.0)
    total = reactive(0.0)

    def on_mount(self) -> None:
        """Event handler called when widget is added to the app."""
        self.timer = self.set_interval(1 / 60, self.update_time, pause=True)

    def update_time(self) -> None:
        """Method to update the time to the current time."""
        self.time = self.total + (monotonic() - self.start_time)

    def watch_time(self, time: float) -> None:
        """Called when the time attribute changes."""
        mins, secs = divmod(time, 60)
        hrs, mins = divmod(mins, 60)
        self.update(f'{hrs:02.0f}:{mins:02.0f}:{secs:05.2f}')

    def start(self) -> None:
        self.start_time = monotonic()
        self.timer.resume()

    def stop(self) -> None:
        self.timer.pause()
        self.total += monotonic() - self.start_time
        self.time = self.total

    def reset(self) -> None:
        """Resets the timer and time display"""
        self.time = 0
        self.total = 0

class Stopwatch(Static):
    """A stopwatch widget."""

    def on_button_pressed(self, event: Button.Pressed) -> None:
        """Event handler for when a button is pressed."""
        time_display = self.query_one(TimeDisplay)
        match event.button.id:
            case "start":
                time_display.start()
                self.add_class("started")
            case "stop":
                time_display.stop()
                self.remove_class("started")
            case "reset":
                time_display.reset()

    def compose(self) -> ComposeResult:
        yield Button("Start", id="start", variant="success")
        yield Button("Stop", id="stop", variant="error")
        yield Button("Reset", id="reset")
        yield TimeDisplay()

class StopwatchApp(App):
    """A simple stopwatch app."""
    BINDINGS = [('d', 'toggle_dark', 'Toggle dark mode')]
    CSS_PATH = "t.tcss"

    def compose(self) -> ComposeResult:
        yield Header()
        yield Footer()
        yield ScrollableContainer(Stopwatch(), Stopwatch())

    def action_toggle_dark(self) -> None:
        """Toggle dark mode."""
        self.dark = not self.dark

if __name__ == '__main__':
    app = StopwatchApp()
    app.run()
