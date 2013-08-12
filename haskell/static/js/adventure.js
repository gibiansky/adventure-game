// Generated by CoffeeScript 1.6.3
/* Models*/


(function() {
  var AdventureView, Command, CommandHistory, CommandView, Sidebar, TextField, _ref, _ref1, _ref2, _ref3, _ref4, _ref5,
    __hasProp = {}.hasOwnProperty,
    __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
    __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };

  Command = (function(_super) {
    __extends(Command, _super);

    function Command() {
      _ref = Command.__super__.constructor.apply(this, arguments);
      return _ref;
    }

    Command.prototype.url = "/run";

    return Command;

  })(Backbone.Model);

  /* Collections*/


  CommandHistory = (function(_super) {
    __extends(CommandHistory, _super);

    function CommandHistory() {
      _ref1 = CommandHistory.__super__.constructor.apply(this, arguments);
      return _ref1;
    }

    CommandHistory.prototype.model = Command;

    CommandHistory.prototype.url = "/history";

    return CommandHistory;

  })(Backbone.Collection);

  /* Views*/


  Sidebar = (function(_super) {
    __extends(Sidebar, _super);

    function Sidebar() {
      this.render = __bind(this.render, this);
      this.update = __bind(this.update, this);
      this.initialize = __bind(this.initialize, this);
      _ref2 = Sidebar.__super__.constructor.apply(this, arguments);
      return _ref2;
    }

    Sidebar.prototype.initialize = function() {
      return this.update();
    };

    Sidebar.prototype.update = function() {
      var _this = this;
      return $.get("/items", function(data) {
        _this.items = $.parseJSON(data);
        return _this.render();
      });
    };

    Sidebar.prototype.render = function() {
      var _this = this;
      this.$el.html('');
      return _.each(this.items, function(item) {
        var itemDiv;
        itemDiv = $(document.createElement('div'));
        itemDiv.attr("class", "item");
        itemDiv.html(item);
        return _this.$el.append(itemDiv);
      });
    };

    return Sidebar;

  })(Backbone.View);

  AdventureView = (function(_super) {
    __extends(AdventureView, _super);

    function AdventureView() {
      this.render = __bind(this.render, this);
      this.createAndRender = __bind(this.createAndRender, this);
      this.addCommand = __bind(this.addCommand, this);
      this.initialize = __bind(this.initialize, this);
      _ref3 = AdventureView.__super__.constructor.apply(this, arguments);
      return _ref3;
    }

    AdventureView.prototype.views = [];

    AdventureView.prototype.initialize = function(options) {
      this.historyView = $(document.createElement('div'));
      this.historyView.attr("id", 'history-view');
      this.$el.append(this.historyView);
      this.textEntry = $(document.createElement('div'));
      this.textEntry.attr("id", 'text-entry');
      this.$el.append(this.textEntry);
      this.history = options.history;
      this.history.bind('add', this.addCommand);
      this.history.bind('change', this.render);
      this.history.bind('reset', this.createAndRender);
      return this.render();
    };

    AdventureView.prototype.addCommand = function(cmd) {
      this.views.push(new CommandView({
        model: cmd
      }));
      return this.render();
    };

    AdventureView.prototype.createAndRender = function(collection) {
      var _this = this;
      _.each(collection.models, function(cmd) {
        return _this.addCommand(cmd);
      });
      return this.render;
    };

    AdventureView.prototype.render = function() {
      var _this = this;
      this.historyView.innerHtml = '';
      _.each(this.views, function(view) {
        return _this.historyView.append(view.render().el);
      });
      this.historyView.scrollTop(this.historyView.prop('scrollHeight'));
      console.log('scroll', this.historyView.prop('scrollHeight'));
      this.historyView.find("img").load(function() {
        console.log('scroll-post-load', _this.historyView.prop('scrollHeight'));
        return _this.historyView.scrollTop(_this.historyView.prop('scrollHeight'));
      });
      return this;
    };

    return AdventureView;

  })(Backbone.View);

  TextField = (function(_super) {
    __extends(TextField, _super);

    function TextField() {
      this.render = __bind(this.render, this);
      this.cursorSplitText = __bind(this.cursorSplitText, this);
      this.keyUp = __bind(this.keyUp, this);
      this.keyDown = __bind(this.keyDown, this);
      this.keyPress = __bind(this.keyPress, this);
      this.submit = __bind(this.submit, this);
      this.initialize = __bind(this.initialize, this);
      _ref4 = TextField.__super__.constructor.apply(this, arguments);
      return _ref4;
    }

    TextField.cursorImgSrc = "/static/imgs/cursor.gif";

    TextField.prototype.cursorHtml = "<img src='" + TextField.cursorImgSrc + "' id='cursor' />";

    TextField.prototype.enteredText = "";

    TextField.prototype.cursorPosition = 0;

    TextField.prototype.initialize = function(options) {
      _.bindAll(this);
      $(document).bind('keypress', this.keyPress);
      $(document).bind('keydown', this.keyDown);
      $(document).bind('keyup', this.keyUp);
      this.history = options.history;
      this.items = options.items;
      this.sidebar = options.sidebar;
      return this.render();
    };

    TextField.prototype.submit = function(text) {
      var command;
      command = new Command({
        command: text,
        response: ""
      });
      this.history.add(command);
      command.save();
      return this.sidebar.update();
    };

    TextField.prototype.keyPress = function(event) {
      var char, postCursor, preCursor, printable, _ref5;
      char = String.fromCharCode(event.which);
      printable = /^[-a-zA-Z0-9 ]$/.test(char);
      if (!printable) {
        return;
      }
      _ref5 = this.cursorSplitText(), preCursor = _ref5[0], postCursor = _ref5[1];
      this.enteredText = preCursor + char + postCursor;
      this.cursorPosition += 1;
      return this.render();
    };

    TextField.prototype.keyDown = function(event) {
      var backspaceKeyCode, deleteKeyCode, leftKeyCode, postCursor, preCursor, rightKeyCode, textLen, _ref5;
      deleteKeyCode = 46;
      backspaceKeyCode = 8;
      leftKeyCode = 37;
      rightKeyCode = 39;
      textLen = this.enteredText.length;
      _ref5 = this.cursorSplitText(), preCursor = _ref5[0], postCursor = _ref5[1];
      switch (event.keyCode) {
        case leftKeyCode:
          if (this.cursorPosition !== 0) {
            this.cursorPosition--;
          }
          break;
        case rightKeyCode:
          if (this.cursorPosition !== textLen) {
            this.cursorPosition++;
          }
          break;
        case deleteKeyCode:
          if (this.cursorPosition !== textLen) {
            this.enteredText = preCursor + postCursor.substring(1);
          }
          break;
        case backspaceKeyCode:
          if (this.cursorPosition !== 0) {
            this.enteredText = preCursor.substring(0, preCursor.length - 1) + postCursor;
            this.cursorPosition--;
          }
      }
      return this.render();
    };

    TextField.prototype.keyUp = function(event) {
      var enterKeyCode, submitting;
      enterKeyCode = 13;
      if (event.which === enterKeyCode) {
        submitting = this.enteredText;
        if (submitting.length > 0) {
          this.enteredText = "";
          this.cursorPosition = 0;
          this.submit(submitting);
          return this.render();
        }
      }
    };

    TextField.prototype.cursorSplitText = function() {
      var postCursor, preCursor;
      preCursor = this.enteredText.substring(0, this.cursorPosition);
      postCursor = this.enteredText.substring(this.cursorPosition);
      return [preCursor, postCursor];
    };

    TextField.prototype.render = function() {
      var postCursor, preCursor, template, text, _ref5;
      _ref5 = this.cursorSplitText(), preCursor = _ref5[0], postCursor = _ref5[1];
      text = preCursor + this.cursorHtml + postCursor;
      template = _.template($('#text-entry-template').html(), {
        text: text
      });
      this.$el.html(template);
      return this;
    };

    return TextField;

  })(Backbone.View);

  CommandView = (function(_super) {
    __extends(CommandView, _super);

    function CommandView() {
      this.render = __bind(this.render, this);
      this.initialize = __bind(this.initialize, this);
      _ref5 = CommandView.__super__.constructor.apply(this, arguments);
      return _ref5;
    }

    CommandView.prototype.tagName = 'div';

    CommandView.prototype.initialize = function() {
      return this.model.bind('change', this.render);
    };

    CommandView.prototype.render = function() {
      var response, template;
      response = this.model.attributes.response;
      if (response.match(/^error:/)) {
        response = "<span class='error'>" + response + "</span>";
      }
      template = _.template($('#command-template').html(), {
        command: this.model.attributes.command,
        response: response.replace(/\n/g, "<br/>")
      });
      this.$el.html(template);
      return this;
    };

    return CommandView;

  })(Backbone.View);

  this.loadGame = function() {
    var adventureView, history, sidebarView, textField;
    history = new CommandHistory([]);
    sidebarView = new Sidebar({
      el: $('#sidebar')
    });
    adventureView = new AdventureView({
      el: $('#main'),
      history: history
    });
    textField = new TextField({
      el: adventureView.textEntry,
      history: history,
      sidebar: sidebarView
    });
    return history.fetch({
      reset: true
    });
  };

}).call(this);
