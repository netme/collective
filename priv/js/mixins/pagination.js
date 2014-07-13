/*!
 * @overview  Collective - Introspection UI for distributed Erlang systems
 * @copyright Copyright (c) 2014, Alexander Koeck
 * @license   Licensed under BSD license
 *            See https://raw.github.com/lx7/collective/master/LICENSE
 */

Collective.PaginationMixin = Em.Mixin.create({
    page: 1,
    perPage: 50,
    paginatedContent: function() {
        var page    = this.get('page');
        var perPage = this.get('perPage');
        var start   = (page - 1 ) * perPage;
        var end     = page * perPage;
        return this.get('arrangedContent').slice(start, end);
    }.property('content.[]', 'arrangedContent.[]', 'page', 'perPage'),
    pages: function() {
        var count  = this.get('pageCount');
        var page   = this.get('page');
        var result = [];
        for (var i=1; i<=count; i++) {
            result.push({ id: i, current: (i==page) });
        } 
        return result;
    }.property('content.[]', 'perPage', 'page'),
    pageCount: function() {
        return Math.ceil(this.get('content.length') / this.get('perPage'));
    }.property('content.[]', 'perPage', 'page'),
    nextPage: function() {
        var count  = this.get('pageCount');
        var page   = this.get('page');
        return (page < count? page + 1: page);
    }.property('content.[]', 'perPage', 'page'),
    previousPage: function() {
        var count  = this.get('pageCount');
        var page   = this.get('page');
        return (page > 1? page - 1: page);
    }.property('content.[]', 'perPage', 'page'),
    actions: {
        selectPage: function(page) {
            this.set('page', page);
        }
    }
});

