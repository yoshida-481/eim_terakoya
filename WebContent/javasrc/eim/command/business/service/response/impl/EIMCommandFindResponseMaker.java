package eim.command.business.service.response.impl;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.http.HttpServletRequest;

import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultDocumentList;
import eim.command.common.util.EIMCommandConstant;

/**
 * findコマンド用レスポンス返却実装クラス
 * 
 *
 */
public class EIMCommandFindResponseMaker extends EIMCommandResponseMakerJSP {

	@Override
	protected void setResponseData(EIMCommandResult resultDataList, HttpServletRequest request){
		request.setAttribute("resultDataList", (EIMCommandResultDocumentList)resultDataList);
	}
	
	@Override
	protected RequestDispatcher getForwardJSP(HttpServletRequest request) {
		return request.getRequestDispatcher(EIMCommandConstant.INFO_RESULT_LIST_JSP);
	}

}
