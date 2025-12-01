import { Model } from 'spinalize'

export const RiskType = Model.extend({
	idAttribute: "type_name"
})
export const RiskCode = Model.extend({
	idAttribute: "risk_code"
})

export const Risk = Model.extend({
	idAttribute: "risk_id",
	url: "/rpc/model/risk",
	validate(attributes, options) {
		const errors = [],
			  required = [ 'risk_code', 'policy_id', 'contract_id']
		required.forEach(key => {
			if (!attributes[key]) {
				errors.push({
					attribute: key,
					error: 'needs to be defined'
				})
			}
		})

		if (errors.length > 0) {
			return errors
		}
	}
		
})

Risk.prototype.toString = function () {
	null
}





