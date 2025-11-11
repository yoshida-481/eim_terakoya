package eim.command.business.service.response.impl;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import eim.command.business.service.response.EIMCommandResponseMaker;
import eim.command.business.service.result.EIMCommandResult;

/**
 * jspによるレスポンス返却抽象テンプレートクラス
 * 
 *
 */
public abstract class EIMCommandResponseMakerJSP implements EIMCommandResponseMaker{
	public void makeResponse(EIMCommandResult resultData, HttpServletRequest request, HttpServletResponse response) throws Exception{
		
		// レスポンスヘッダー設定
		setResponseHeader(response);
		
		// レスポンスデータ設定
		setResponseData(resultData, request);
		
		// フォワード先jsp取得
		RequestDispatcher rd = getForwardJSP(request);
		
		// jsp経由でレスポンス返却
		rd.forward(request,response);
	}
	/**
	 * レスポンスヘッダに値を設定
	 * @param response
	 * @throws Exception
	 */
	protected void setResponseHeader (HttpServletResponse response) throws Exception{
		//Set Content Type
		response.setContentType("text/xml; charset=utf-8");
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	}
	
	/**
	 * レスポンスデータ設定
	 * @param resultData
	 * @param request
	 */
	abstract protected void setResponseData(EIMCommandResult resultData, HttpServletRequest request);
	
	/**
	 * フォワード先jsp取得
	 * @param request
	 * @return
	 */
	abstract protected RequestDispatcher getForwardJSP(HttpServletRequest request);
}
