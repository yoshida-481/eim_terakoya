package eim.command.business.service.response.impl;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.http.HttpServletRequest;

import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.EIMCommandConstant;

public class EIMCommandErrorResponseMaker extends EIMCommandResponseMakerJSP {

	@Override
	protected void setResponseData(EIMCommandResult resultData, HttpServletRequest request){
		request.setAttribute("resultData", resultData);
	}
	
	@Override
	protected RequestDispatcher getForwardJSP(HttpServletRequest request) {
		return request.getRequestDispatcher(EIMCommandConstant.ERROR_RESULT_JSP);
	}

}
