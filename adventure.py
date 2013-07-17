#!/usr/bin/env python
import json

from flask import Flask
from flask import render_template
from flask import request
from flaskext.coffee import coffee

from game import Game

# Create Flask application.
app = Flask(__name__)
game = Game("rooms")


@app.route("/")
def home():
    """Main application route."""
    return render_template('base.html')


@app.route("/run", methods=['POST'])
def run():
    """Run a command. Return the response as JSON."""
    command = request.json['command']
    response = game.run(command)
    return json.dumps({
        'id': response.id,
        'command': response.command,
        'response': response.output,
    })


@app.route("/history", methods=['GET'])
def history():
    """Get the command history."""
    return json.dumps([{
        'id': response.id,
        'command': response.command,
        'response': response.output,
    } for response in game.history()])

# If this is run via command-line, start a debug server on port 5000.
if __name__ == "__main__":
    # Automatically compile coffeescript files.
    coffee(app)

    app.run('0.0.0.0', debug=True)
