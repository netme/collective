/*!
 * @overview  Collective - Introspection UI for distributed Erlang systems
 * @copyright Copyright (c) 2014, Alexander Koeck
 * @license   Licensed under BSD license
 *            See https://raw.github.com/lx7/collective/master/LICENSE
 */

Collective.Process = Ember.Object.extend({
    id:                 "",
    node:               "",
    name:               "",
    initial_fun:        "",
    current_fun:        "",
    trap_exit:          false,
    reds:               0,
    reds_last:          0,
    reds_delta:         0,
    memory:             0,
    msgq:               0,
    group_leader:       "",
    links:              [], 
    monitors:           [], 
    
    init: function() {
        this.set('links', Ember.A());
    },
    compute: function() {
        var reds = this.get('reds');
        var last = this.get('reds_last');
        this.set('reds_last', reds);
        this.set('reds_delta', reds - last);
    },
    type: function() {
        if (this.get('group_leader') === this.get('id')) {
            return 'group_leader';
        } else {
            return 'default';
        }
    }.property('group_leader')
});

