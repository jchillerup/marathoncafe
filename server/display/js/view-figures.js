var PlotsView = Backbone.View.extend({
    id: "plots",
    cur_plot: 0,
    $cur_img: null,
    plotfiles: ['plot0.png', 'plot2-kumulerede-antal-streger.png', 'plot5-streger-paa-barvagt.png', 'plot7-cum-top5.png', 'plot900-hamringsmomentum-global.png'],

    events: {
        "click img": "advance"
    },
    
    initialize: function() {
        console.log('plots view initialized');
        
        this.insertPic();
        this.insertPic();
        this.advance();

        setInterval(_.bind(this.advance, this), 18000);
    },
    
    insertPic: function() {
        var path = "plots/"+(this.plotfiles[this.cur_plot])+"?cachebust="+(new Date()).getTime();
        var $img = $("<img>").attr('src', path).css('left', '100%');

        this.$el.append($img);
        this.$cur_img = $img;
    },

    advance: function(event) {
        this.cur_plot += 1;
        this.cur_plot %= this.plotfiles.length;
        
        this.insertPic();

        this.$("img:first").animate({"left": "-480px"}, 600, function() {$(this).remove();});
        $(this.$("img").get(1)).animate({"left": "0px"}, 600);       
    }
});
