import { Contract } from './model'
import { Contracts } from './collection'
import { Autocomplete, View, M, $, _, Backbone } from 'spinalize'

export const InlineContract = View.extend({
	initialize() {
		this.render()
	},
	render() {
		const a = _.template('<a href="/ecm/view?contract=<%- c.id %>"><%- c.get("contract_number") %></a>',
							 { variable: 'c'})(this.model)
		console.info('got a?', a)
		this.$el.html('asd' + a)
	}
})


export const SelectContract = Autocomplete.extend({
	initialize(...args) {
		Autocomplete.prototype.initialize.call(this, ...args)
		if (this.collection) {
			this.loadMenu()
		} else {
			const contracts = new Contracts()
			contracts.load().then(cs => {
				this.collection = contracts;
				this.loadMenu()
			})
		}
		// 	this.onAutocomplete(...arguments)
		// }
	},
	loadMenu() {
		const data = this.collection.map(c => {
			return { id: c.id, text: c.toString() }
		})

		console.warn('data from collection', data)
		this.setMenuItems(data, null, false)
		this.instance.options.minLength = 0
	}
})




