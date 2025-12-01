import { Model } from 'spinalize'

export const Policy = Model.extend({
	idAttribute: "policy_id"
})

const createNames = (policy, names) => {
	// console.debug("getting", names, 'from', policy)
	return names.map(n => policy[n]).filter(n => !!n)
		.map(n => n.name).join(', ')
}
	
Policy.prototype.toString = function () {
	const p = this.attributes,
		  names = ['agent', 'insurance_company', 'broker', 'agency_office'],
		  ns = createNames(p, names),
		  ef = p.effective_date,
		  efd = ef && new Date(ef).toLocaleDateString('en-CA'),
		  ex = p.expiry_date,
		  exd = ex && new Date(ex).toLocaleDateString('en-CA')
	
	return `${p.policy_number}, ${p.insured.name} | ${ns} | ${efd} - ${exd}`
}






