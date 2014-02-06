var ScoreModel = Backbone.Model.extend({
    endpoint: "http://localhost:8081/",
    socket: null,

    initialize: function(attr, opts) {
        this.socket = opts.socket;

        this.set('quantity', 0);
        this.on('change:kitchen', _.bind(this.submit, this));
    },

    submit: function() {
        if (this.get('kitchen') !== undefined) {
            this.socket.emit('streg', this.toJSON());
            console.log(this.toJSON());

            // HACK
            this.clear();
            this.set('quantity', 0);
            // /HACK
        }
    }
});

var KitchenPicker = Backbone.View.extend({
    events: {
        "mousedown .kitchenButton": "setScore"
    },

    initialize: function() {
        this.$buttons = this.$('button');

        this.model.on('change:quantity', _.bind(this.render, this));
    },

    render: function() {
        if (this.model.get('quantity') === 0) {
            this.$buttons.attr('disabled', 'disabled');
        } else {
            this.$buttons.removeAttr('disabled');
        }
    },
    
    setScore: function(el) {
        var kitchen = el.target.id;
        this.model.set('kitchen', kitchen);
    }

});

var POSView = Backbone.View.extend({
    id: "posView",
    initialize: function() {
        this.model.on('change', _.bind(this.render, this));
        this.render();
    },
    
    render: function() {
        this.$el.html(this.model.get('quantity') + " streger");
    }
});

var socket = io.connect('/');
var curScore = new ScoreModel(null, {socket: socket});    

$(function() {    
    var kitchenView = new KitchenPicker({ el: document.getElementById("kitchens"), model: curScore });
    var posView = new POSView({model: curScore});

    $('#display h1').after(posView.$el);

    $("#orders button").on('mousedown startdrag', function() {
        if (this.id === "clearButton") {
            curScore.set('quantity', 0);
        } else {
            var score = this.value;
            curScore.set('quantity', parseFloat(curScore.get('quantity')) + parseFloat(score));
        }
    });
});


function testPOS(interval) {
    if(interval === undefined) {
        interval = 1000;
    }
    
    var buildings = ["GL", "ML", "NY"];
    
    function makeOrder() {
        curScore.set({kitchen: buildings[Math.floor(Math.random()*buildings.length)]+Math.floor((Math.random()*7)+2), quantity: Math.floor((Math.random()*10)+1)});
        curScore.submit();
    }
    
    return setInterval(makeOrder, interval);
}