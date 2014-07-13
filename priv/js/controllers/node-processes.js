/*!
 * @overview  Collective - Introspection UI for distributed Erlang systems
 * @copyright Copyright (c) 2014, Alexander Koeck
 * @license   Licensed under BSD license
 *            See https://raw.github.com/lx7/collective/master/LICENSE
 */

Collective.NodeProcessesController = Ember.ArrayController.extend(
    Collective.PaginationMixin, {
    sortProperties: ['reds_delta'],
    sortAscending: false,
    actions: {
        sortBy: function(prop, asc) {
            self = this;
            Ember.run(function() {
                self.set('sortProperties', [prop]);
                self.set('sortAscending', asc);
            });
        }
    }
});

