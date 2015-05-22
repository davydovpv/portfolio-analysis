var ComparisonList = Backbone.Collection.extend({
    addPortfolio: function (attr) {
        var model = new Backbone.Model(attr);
        this.add(model);
    }
});

var ComparisonTableView = Backbone.View.extend({
    rowTemplate: Handlebars.compile('<tr><td>{{name}}</td><td>{{weights}}</td><td>{{start}}</td><td>{{end}}</td><td>{{mean}}</td><td>{{sd}}</td></tr>'),
    initialize: function () {
        this.listenTo(this.collection, 'add', this.add);

        this.datatable = $('#compare-table').DataTable({
            "paging": false,
            "ordering": true,
            "info": false,
            "searching":false
        });
    },
    add: function (model) {
        this.datatable.row.add([
            model.get('name'),
            model.get('weights'),
            model.get('start'),
            model.get('end'),
            model.get('mean'),
            model.get('sd')
        ]).draw();
    }
});

var comparisonList = new ComparisonList();

$(function () {
    var comparisonTable = new ComparisonTableView({
        el: '#compare-portfolio-view',
        collection: comparisonList
    });

    $('#save-portfolio').click(function () {
        var name = prompt('Enter a name for this portfolio.', 'My Portfolio'),
            weights = selectedSymbols.getWeightsString(),
            start = $('.date-input.left').val(),
            end = $('.date-input.right').val();

        var mean = 0,
            sd = 0;

        try {
            mean = +$('#statisticsPortfolio table tr').eq(1).find('td').eq(1).text().replace('%', '').trim(),
            sd = +$('#statisticsPortfolio table tr').eq(1).find('td').eq(2).text().replace('%', '').trim();
        } catch (e) {}          
        
        comparisonList.addPortfolio({
            name: name,
            weights: weights,
            mean: mean,
            sd: sd,
            start: start,
            end: end
        });


    });
});