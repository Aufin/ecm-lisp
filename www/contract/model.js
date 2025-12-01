import { Model } from 'spinalize'

export const Contract = Model.extend({
	idAttribute: "contract_id"
})

Contract.prototype.toString = function () {
	const c = this.attributes
	return `${c.contract_number} ${c.effective_date} to ${c.expiry_date}: ${c.agency || ''}${c.syndicate_id ? ', ' + c.syndicate : ''}${c.london_broker_id ? ', ' + c.london_broker : ''}`
}






