match command.split():
    case ["go", direction] if direction in current_room.exits:
        current_room = current_room.neighbor(direction)
    case ["go", _]:
        print("Sorry, you can't go that way")
    case ["go", *rest]:
        print("Sorry, you can't go that way")
    case Click(position=(x, y)):
        handle_click_at(x, y)
    case KeyPress(key_name="Q") | Quit():
        game.quit()
    case KeyPress(key_name="up arrow"):
        game.go_north()
    case KeyPress():
        pass # Ignore other keystrokes
    case long.value.pattern:
        pass
    case other_event:
        raise ValueError(f"Unrecognized event: {other_event}")