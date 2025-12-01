// -*- mode: js -*-
import * as S from 'spinalize'
import 'spinalize/css'

import {
	Contract,
	Contracts,
	SelectContract,
	InlineContract
} from './contract/index.js'

import {
	Policy, Policies, SelectPolicy
} from './policy/index.js'


import {
	Risk, SelectRiskPolicy, SelectRiskContract,
	RiskType, RiskTypes, SelectRiskType,
	RiskCode, RiskCodes, SelectRiskCode,
	SubmitRisk, EditRisk
} from './risk/index.js'

const model = {
	Contract,
	Policy,
	Risk
}

const collection = {
	Contracts,
	Policies,
	RiskTypes
}

const view = {
	SelectContract, InlineContract,
	SelectRiskContract,
	SelectPolicy, SelectRiskPolicy,
	SelectRiskType, SelectRiskCode,
	SubmitRisk, EditRisk
	
}



export {
	S,
	model, collection, view
	
}
