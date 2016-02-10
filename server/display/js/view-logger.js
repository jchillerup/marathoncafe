function getTimestamp() {
    var d = new Date();
    var ts = "";
    var h = d.getHours(), m = d.getMinutes(), s = d.getSeconds();
    
    if (h < 10) {
        ts += "0";
    } 
    ts += "" + h + ":";
    
    if (m < 10) {
        ts += "0";
    } 
    ts += "" + m; 

    return ts;
}

var Logger = Backbone.View.extend({
    tagName: "ul",
    id: "logList",
    maxelements: 50,
    
    initialize: function() {
        this.model.on('change', _.bind(this.newLogEntry, this));
    },
    
    newLogEntry: function() {
        var attributes = this.model.changedAttributes();
        var prevAttributes = this.model.previousAttributes();

        for (var kitchen in attributes) {
            var diff = attributes[kitchen] - prevAttributes[kitchen];
            diff = diff.toFixed(2).replace(".00", "");
            var $obj = $("<li>").html("<span class=\"timestamp\">" + getTimestamp() + "</span> " + kitchen+ ": "+ diff);

            this.$el.prepend($obj.fadeIn());
        }
        this.render();
    },

    render: function() {
        while (this.$el.children().length > this.maxelements) {
            $(this.$el.children()[this.maxelements]).fadeOut().remove();
        }
    }
});