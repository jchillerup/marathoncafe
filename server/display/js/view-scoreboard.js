var Scoreboard = Backbone.View.extend({
    tagName: "section",
    id: "scoreboard",
    children: [],

    initialize: function() {
        this.model.on('change', _.bind(this.render, this));
                
        // Create all the kitchen widgets
        var kitchens = [
            "GL1", 
            "GL2", "ML2", "NY2",
            "GL3", "ML3", "NY3",
            "GL4", "ML4", "NY4",
            "GL5", "ML5", "NY5",
            "GL6", "ML6", "NY6",
            "GL7", "ML7", "NY7",
            "GL8", "ML8", "NY8"
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
        this.render();
    },
    render: function() {
        console.log(this.kitchen_id + " render" );
        this.$el.html(this.kitchen_id + " <span class=\"points\">" + this.model.get(this.kitchen_id) + "</span>");
    }
});