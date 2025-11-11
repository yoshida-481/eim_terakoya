package eim.command.business.service.response.impl;


import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.http.HttpServletRequest;

import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.EIMCommandConstant;

/**
 * checkinByUser用レスポンス返却実装クラス
 *
 */

public class EIMCommandCheckinByUserResponseMaker extends EIMCommandResponseMakerJSP{

	@Override
	protected RequestDispatcher getForwardJSP(HttpServletRequest request) {
		return request.getRequestDispatcher(EIMCommandConstant.INFO_RESULT_JSP);
	}

	@Override
	protected void setResponseData(EIMCommandResult resultData,HttpServletRequest request) {
		request.setAttribute("resultData", resultData);
	}

}
