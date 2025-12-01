import { Collection } from '../collection'
import { Risk, RiskType, RiskCode } from './model'

export const RiskTypes = Collection.extend({
	model: RiskType,
	url: '/rpc/collection/risk-types'
})

export const RiskCodes = Collection.extend({
	model: RiskCode,
	url: '/rpc/collection/risk-codes'
})


	


