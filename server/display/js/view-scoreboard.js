var Scoreboard = Backbone.View.extend({
    tagName: "section",
    id: "scoreboard",
    children: [],

    initialize: function() {
        this.model.on('change', _.bind(this.render, this));
        
        
        // Create all the kitchen widgets
        var kitchens = [
            "G1", 
            "G2", "M2", "N2",
            "G3", "M3", "N3",
            "G4", "M4", "N4",
            "G5", "M5", "N5",
            "G6", "M6", "N6",
            "G7", "M7", "N7",
            "G8", "M8", "N8"
        ];

        for (var kitchen in kitchens) {
            var unit = new ScoreboardUnit({
                model: this.model, 
                kitchen_id: kitchens[kitchen],
                id: "score-"+kitchens[kitchen]
            });
            
            this.children.push(unit);
            this.$el.append(unit.$el);
        }

    },

    render: function() {
        console.log("scoreboard render");

        this.$el.children().tsort('span.points', {order: 'desc'});
    }
});

var ScoreboardUnit = Backbone.View.extend({
    tagName: "div",
    className: "scoreboard-unit",
    
    initialize: function() {
        this.kitchen_id = this.id.split("-")[1];
        
        if (!this.model.has(this.kitchen_id)) {
            this.model.set(this.kitchen_id, 0);
        }

        this.model.on('change:'+this.kitchen_id, _.bind(this.render, this));
    },
    render: function() {
        console.log(this.kitchen_id + " render" );
        this.$el.html(this.kitchen_id + " <span class=\"points\">" + this.model.get(this.kitchen_id) + "</span>");
    }
});