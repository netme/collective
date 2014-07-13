/*!
 * @overview  Collective - Introspection UI for distributed Erlang systems
 * @copyright Copyright (c) 2014, Alexander Koeck
 * @license   Licensed under BSD license
 *            See https://raw.github.com/lx7/collective/master/LICENSE
 */

Collective.NodesController = Ember.ArrayController.extend({
    sortProperties: ['id'],
    sortAscending: true
});

