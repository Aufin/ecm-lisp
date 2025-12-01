import { Collection } from '../collection'
import { Policy } from './model'

export const Policies = Collection.extend({
	model: Policy,
	url: '/rpc/collection/policies',
	search(term, limit = 20, offset = false) {
		return this.fetch({ term, limit, offset})
		// console.warn("searching for", term, limit, offset)
		// return fetch(this.url, {
		// 	method: "POST",
		// 	body: JSON.stringify({
		// 		term, limit, offset
		// 	})
		// }).then(r =>r.json()).then(response => {
		// 	console.warn("Got response", response)
		// 	this.response = response;
		// 	this.add(response.results)
		// 	return response;
		// })
	},
		
								
		
		
})


	


