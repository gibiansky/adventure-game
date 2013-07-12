from collections import defaultdict

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

class CommandNotFoundError(ValueError):
    def __init__(self):
        super(CommandNotFoundError, self).__init__("No such command.")

def no_such_command():
    raise CommandNotFoundError()

def get_response(responses, index):
    if index >= len(responses):
        return responses[-1]
    else:
        return responses[index]


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

        self.started = False
        self.powers = []
        self.location = None
        self.command_counts = defaultdict(int)

        self.run("init")

    def gain_power(self, power, phrase="You have gained the power to '%s'"):
        self.powers.append(power)

        return phrase % power

    def lose_power(self, power):
        ind = self.powers.index(power)
        del self.powers[ind]

    def move_to(self, loc):
        self.location = loc

    def run(self, command_str):
        for cmd in COMMANDS:
            if command_str.startswith(cmd + " ") and cmd in self.powers:
                args = command_str[len(cmd) + 1:].split()
                response = COMMANDS[cmd](self, args)
                self.command_counts[cmd] += 1
                break
        else:
            msg = "error: no matching command found. enqueue headpat."
            response = Response(command_str, msg)

        self.command_history.append(response)
        return response

    def history(self):
        return self.command_history

@command("init")
def start(game, *args):
    """Command implicitly called on game start."""
    if game.started:
        raise CommandNotFoundError()

    game.started = True

    output = "\n".join(("You are on in a strange room.",
                        "[room description]",
                        "Are you okay with that?"))

    output += game.gain_power("yes", "You have learned to say '%s'.")
    output += game.gain_power("no", "You have also learned to say '%s'.")

    game.move_to("first-room-1")

    return output

@command()
def yes(game, *args):
    if game.location == "first-room-1":
        responses = [
            "Good! You didn't really have a choice anyway.",
            "Good! I'm glad you're so agreeable today."
        ]
        second_responses = [
            "You're learning.",
            "Since you've started agreeing with me, why not continue?"
        ]
        first = get_response(responses, game.command_counts["no"])
        second = get_response(second_responses, game.command_counts["no"])

        game.move_to("first-room-2")

        return "%s\n\n%s Let's try the next one." % (first, second)

@command()
def no(game, *args):
    if game.location == "first-room-1":
        responses = [
            "You should really be more okay with it. Try again.",
            "Are you sure that's a good idea? It might lead to some serious " +
            "cognitive dissonance issues later on. Try another answer.",
            "Maybe you should reconsider saying 'no' again and just say 'yes'.",
            "I'm not letting you go anywhere until you say yes. So just say" +
            " 'yes'.",
        ]
        return get_response(responses, game.command_counts["no"])
    elif game.location == "first-room-2":
        "I'm can't let you do that, Da-- Tasha.",
              "YOU. SHALL. NOT. PASS. Erm. Say no. Whatever. FLEE YOU FOOLS.",
              'right_answer = "yes"; me = &right_answer; // Pointing you in   the ' + 'right direction.'
responses = [
        
        ]


