from collections import defaultdict
import os

from room import Room


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


def matches(template, values):
    def word_match((temp, val)):
        if temp == "*":
            return True

        if temp.startswith("^") and not val == temp[1:]:
            return True

        if temp == val:
            return True

        return False

    if len(template) != len(values):
        return False

    return all(map(word_match, zip(template, values)))


class Game(object):
    def __init__(self, roomdir):
        self.command_history = []

        self.powers = []
        self.location = None
        self.command_counts = defaultdict(int)

        self.rooms = [Room(roomdir + "/" + filename)
                      for filename in os.listdir(roomdir)]

        start_text = self.move_to('init')
        first_response = Response("start", start_text)
        self.command_history.append(first_response)

    def gain_power(self, power):
        if power not in self.powers:
            self.powers.append(power)
            return True
        else:
            return False

    def lose_power(self, power):
        self.powers.remove(power)

    def move_to(self, loc):
        # Reset command counts.
        self.command_counts = defaultdict(int)

        out = []

        if self.location is not None:
            for command in self.location.exit:
                out.append(command.execute(self))

        for room in self.rooms:
            if room.name == loc:
                self.location = room
                break
        else:
            raise ValueError("No such room: " + loc)

        for command in self.location.enter:
            out.append(command.execute(self))

        return "\n".join(out)

    def run(self, command_str):
        print self.powers
        for power in self.powers:
            if command_str.startswith(power):
                args = command_str[len(power) + 1:].split()

                out = []
                for command_set, desired_args in self.location.power[power]:
                    print power, args, desired_args
                    if matches(desired_args, args):
                        for command in command_set:
                            out.append(command.execute(self))

                        response = Response(command_str, "\n".join(out))
                        self.command_counts[power] += 1
                        break
                else:
                    msg = "error: invalid arguments. enqueue headpat."
                    response = Response(command_str, msg)

                break
        else:
            msg = "error: no matching command found. enqueue headpat."
            response = Response(command_str, msg)

        self.command_history.append(response)
        return response

    def history(self):
        return self.command_history
