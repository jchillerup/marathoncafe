var ScoreModel = Backbone.Model.extend({
    socket: null,
    defaults: {
	quantity: 0,
	beer: 0,
	drink: 0,
	jbomb: 0,
	fisk: 0,
	shot: 0},

    initialize: function(attr, opts) {
        this.socket = opts.socket;

        this.set(this.defaults);
        this.on('change:kitchen', _.bind(this.submit, this));
    },

    submit: function() {
        if (this.get('kitchen') !== undefined) {
            this.socket.emit('streg', this.toJSON());
            console.log(this.toJSON());

            // HACK
            this.clear();
            this.set(this.defaults);
            // /HACK
        }
    }
});

var KitchenPicker = Backbone.View.extend({
    events: {
        "click .kitchenButton": "setScore",
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

var socket = io.connect();
var curScore = new ScoreModel(null, {socket: socket});    

$(function() {    
    // Initialize FastClick
    //FastClick.attach(document.body);

    var kitchenView = new KitchenPicker({ el: document.getElementById("kitchens"), model: curScore });
    var posView = new POSView({model: curScore});

    $('#display h1').after(posView.$el);

    $("#orders button").on('click', function(event, button) {
	var product = $(event.target).data('order');
	var unit_quantity = $(event.target).data('quantity');
	
        if (this.id === "clearButton") {

	    curScore.set(curScore.defaults);
	    
        } else {
            var score = this.value;
            curScore.set('quantity', parseFloat(curScore.get('quantity')) + parseFloat(score));

	    curScore.set(product, (curScore.get(product)||0)+unit_quantity);
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

function randomRHK() { socket.emit('streg', {kitchen: Math.floor(Math.random()*7+1)+ ["A","B","C","D"][Math.floor(Math.random()*4)], quantity: Math.floor(Math.random()*8+1)}); }
