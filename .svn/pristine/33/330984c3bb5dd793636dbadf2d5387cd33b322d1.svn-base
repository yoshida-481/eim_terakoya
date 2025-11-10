package eim.command.business.service.response;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import eim.command.business.service.result.EIMCommandResult;

public interface  EIMCommandResponseMaker {

	/**
	 * 実行結果格納インスタンスを元に、レスポンスを作成・返却
	 * @param resultData
	 * @param response
	 * @throws Exception
	 */
    public void makeResponse(EIMCommandResult resultData, HttpServletRequest request, HttpServletResponse response) throws Exception;

}
