class Response(object):
    """Response from running a command."""
    response_id = 1
    def __init__(self, cmd, output):
        super(Response, self).__init__()

        # Get the next id.
        self.id = Response.response_id
        Response.response_id += 1

        self.command = cmd
        self.output = output


COMMANDS = {}
def command(*aliases):
    def decorator(function):
        def cmd_func(args, *other, **kwargs):
            cmd = function.__name__ + " " + " ".join(args)
            out = function(args, *other, **kwargs)

            return Response(cmd, out)

        # Make bindings for this command in the global command.
        COMMANDS[function.__name__] = cmd_func
        for name in aliases:
            COMMANDS[name] = cmd_func

        return cmd_func
    return decorator

class Game(object):
    def __init__(self):
        self.command_history = []

    def run(self, command_str):
        for cmd in COMMANDS:
            if command_str.startswith(cmd + " "):
                args = command_str[len(cmd) + 1:].split()
                response = COMMANDS[cmd](args)
                break
        else:
            msg = "error: no matching command found. enqueue headpat."
            response = Response(command_str, msg)

        self.command_history.append(response)
        return response

    def history(self):
        return self.command_history

@command("test", "go to", "hello")
def goodbye(arguments):
    return "Args: " + str(arguments)
