var Logger = Backbone.View.extend({
    tagName: "ul",
    id: "logList",
    
    initialize: function() {
        this.model.on('change', _.bind(this.newLogEntry, this));
    },
    
    newLogEntry: function() {
        var attributes = this.model.changedAttributes();
        var prevAttributes = this.model.previousAttributes();

        for (var kitchen in attributes) {
            var diff = attributes[kitchen] - prevAttributes[kitchen];
            var $obj = $("<li>").html(kitchen+ ": "+ diff + " streger.");

            this.$el.append($obj);
            
            while (this.$el.children().length > 7) {
                $(this.$el.children()[0]).remove();
            }
        }
    },

    render: function() {
        
    }
});