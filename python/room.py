from collections import defaultdict

COMMAND_TYPES = {}
SECTION_TYPES = {}


def command(cmdtype):
    def decorator(cls):
        COMMAND_TYPES[cmdtype] = cls
        return cls
    return decorator


def section(sectiontype):
    def decorator(cls):
        SECTION_TYPES[sectiontype] = cls
        return cls
    return decorator


class Command(object):
    def __init__(self, header, has_args=False, nests=False):
        super(Command, self).__init__()

        self.lines = []
        self.arguments = []
        self.has_args = has_args
        self.nests = nests

        if ':' in header:
            argstart = header.index(':') + 1
            args = header[argstart:].strip().split()
            self.arguments.extend([arg.strip() for arg in args])

    def line(self, line):
        self.lines.append(line)

    def execute(self, game):
        raise NotImplementedError("execute() not implemented for " + str(self))

    def parse(self):
        self.arguments.append("\n".join(self.lines))

    def can_take_arguments(self, args):
        pass


@command("respond")
class OutputCommand(Command):
    def execute(self, game):
        return self.arguments[0]


@command("gain")
class GainPowerCommand(Command):
    def execute(self, game):
        power = self.arguments[0]
        phrase = "You have gained the power to '_'."
        if len(self.arguments) > 1 and len(self.arguments[1].strip()) > 0:
            phrase = self.arguments[1]
        print 'huh', phrase, power

        gained = game.gain_power(power)
        if not gained:
            return ""

        return phrase.replace("_", power)


@command("lose")
class LosePowerCommand(Command):
    def execute(self, game):
        power = self.arguments[0]
        phrase = ""
        if len(self.arguments) > 1:
            phrase = self.arguments[1]

        game.lose_power(power)

        return phrase.replace("_", power)


@command("move-to")
class ChangeRoomsCommand(Command):
    def execute(self, game):
        return game.move_to(self.arguments[0])


@command("choose-by-count")
class ChooseByCountCommand(Command):
    def parse(self):
        options = []
        for line in self.lines:
            if line.startswith("- "):
                options.append(line[2:])
            else:
                options[-1] += "\n" + line

        self.power = self.arguments[0]
        self.options = options

    def execute(self, game):
        index = game.command_counts[self.power]

        if index >= len(self.options):
            return self.options[-1]
        else:
            return self.options[index]


@command("on")
class IfCommand(Command):
    def __init__(self, header):
        super(IfCommand, self).__init__(header, nests=True)

        self.subcommands = []

    def can_take_arguments(self, args):
        if len(args) == 0:
            return

        self.has_args = True
        self.argnames = args

    def subcommand(self, subcmd):
        self.subcommands.append(subcmd)

    def should_run(self, args, something_ran):
        if len(self.arguments) == 1:
            if self.arguments[0] == "anything":
                return True
            if self.arguments[0] == 'other':
                return not something_ran
        else:
            return True

    def execute(self, game, args):
        out = []
        for subcommand in self.subcommands:
            if subcommand.has_args:
                out.append(subcommand.execute(game, args))
            else:
                out.append(subcommand.execute(game))
        return "\n".join(out)


class IfGroup(Command):
    def __init__(self, subcommands):
        super(IfGroup, self).__init__("", has_args=True)

        self.subcommands = subcommands

    def execute(self, game, args):
        has_run = False
        out = []

        for subcommand in self.subcommands:
            if subcommand.should_run(args, something_ran=has_run):
                has_run = True
                if subcommand.has_args:
                    out.append(subcommand.execute(game, args))
                else:
                    out.append(subcommand.execute(game))

        return "\n".join(out)


def create_command(command_header):
    for commandstr, commandclass in COMMAND_TYPES.items():
        if command_header.startswith(commandstr):
            return commandclass(command_header)


class RoomSection(object):
    def __init__(self, header):
        super(RoomSection, self).__init__()

        self.lines = []
        self.arguments = []

        if ':' in header:
            argstart = header.index(':') + 1
            args = header[argstart:].strip().split()
            self.arguments.extend([arg.strip() for arg in args])

    def line(self, line):
        self.lines.append(line)

    def parse(self, room):
        commands = []

        for line in self.lines:
            if line.startswith("[") and line.endswith("]"):
                commands.append(create_command(line[1:-1]))
            else:
                commands[-1].line(line)

        for command in commands:
            command.parse()

        self.commands = commands


@section("enter")
class EnterSection(RoomSection):
    def __init__(self, header):
        super(EnterSection, self).__init__(header)
        self.lines.append("[respond]")

    def parse(self, room):
        super(EnterSection, self).parse(room)

        for command in self.commands:
            room.onEnter(command)


@section("exit")
class ExitSection(RoomSection):
    def __init__(self, header):
        super(ExitSection, self).__init__(header)
        self.lines.append("[respond]")

    def parse(self, room):
        super(ExitSection, self).parse(room)

        for command in self.commands:
            room.onExit(command)


@section("power")
class PowerSection(RoomSection):
    def parse(self, room):
        super(PowerSection, self).parse(room)

        self.power = self.arguments[0]

        # Get the arguments for this power.
        self.command_arguments = []
        for arg in self.arguments[1:]:
            if arg.startswith("(") and arg.endswith(")"):
                self.command_arguments.append(arg[1:-1])

        """
        for command in self.commands:
            command.can_take_arguments(self.command_arguments)

        # Allow for some commands to be nested.
        nesting = any(command.nests for command in self.commands)
        if nesting:
            levels = []
            for command in self.commands:
                if len(levels) == 0 or not levels[-1].nests or command.nests:
                    levels.append(command)
                else:
                    levels[-1].subcommand(command)
            self.commands = [IfGroup(levels)]
            """

        room.onPower(self.power, self.commands, self.arguments[1:])


def create_room_section(section_header):
    for startstr, sectionclass in SECTION_TYPES.items():
        if section_header.startswith(startstr):
            return sectionclass(section_header)


def parse_room(room, description):
    # Get all the lines from the description file.
    # Do not include lines that are just whitespace.
    # Remove all extraneous indentation.
    lines = [line.strip() for line in description.split("\n")]

    # Get rid of extra lines before commands.
    filtered = []
    for line, next in zip(lines, lines[1:]):
        if len(line) == 0:
            if not next.startswith("<") and not next.startswith("["):
                filtered.append(line)
        else:
            filtered.append(line)
    filtered.append(lines[-1])
    lines = filtered

    # Split this into sections, denoted by <angle bracket commands>.
    sections = []
    for line in lines:
        if line.startswith("<") and line.endswith(">"):
            sections.append(create_room_section(line[1:-1]))
        else:
            sections[-1].line(line)

    for section in sections:
        section.parse(room)


class Room:
    def __init__(self, filename):
        self.enter = []
        self.exit = []
        self.power = defaultdict(list)

        self.name = filename.split('/')[-1].split(".")[0]
        with open(filename, "r") as roomfile:
            room_description = roomfile.read()

        parse_room(self, room_description)

    def __repr__(self):
        return "Room(%s)" % self.name

    def onEnter(self, cmd):
        self.enter.append(cmd)

    def onExit(self, cmd):
        self.exit.append(cmd)

    def onPower(self, power, cmds, arguments):
        self.power[power].append((cmds, arguments))
