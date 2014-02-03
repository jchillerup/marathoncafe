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
        "click .kitchenButton": "setScore"
    },

    initialize: function() {
        console.log('kitchen picker initialized');
        
        this.$buttons = this.$('button');

        this.model.on('change:quantity', _.bind(function() {
            if (this.model.get('quantity') === 0) {
                this.$buttons.attr('disabled', 'disabled');
            } else {
                this.$buttons.removeAttr('disabled');
            }
        }, this));
    },

    render: function() {},
    
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

$(function() {
    var socket = io.connect('/');
    var curScore = new ScoreModel(null, {socket: socket});
    
    var kitchenView = new KitchenPicker({ el: document.getElementById("kitchens"), model: curScore });
    
    var posView = new POSView({model: curScore});
    $('body').append(posView.$el);

    $("#orders button").click(function() {
        var score = this.value;

        curScore.set('quantity', parseFloat(curScore.get('quantity')) + parseFloat(score));
    });
});
