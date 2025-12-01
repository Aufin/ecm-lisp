import { Collection } from 'spinalize'
import { Contract } from './model'

export const Contracts = Collection.extend({
	model: Contract,
	url: '/rpc/collection/contracts'
})

Contracts.prototype.load = async function (options) {
	const contracts = await fetch(this.url).then(c => c.json())
	return this.reset(contracts, options)
}
	
	


