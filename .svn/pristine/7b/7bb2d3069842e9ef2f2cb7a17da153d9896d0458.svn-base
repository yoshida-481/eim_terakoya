package eim.command.business.service.response.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.OutputStream;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import eim.command.business.service.response.EIMCommandResponseMaker;
import eim.command.business.service.result.EIMCommandResult;

/**
 * バイナリ形式のレスポンス返却抽象テンプレートクラス
 *
 *
 */
public abstract class EIMCommandResponseMakerBinary implements EIMCommandResponseMaker {
	
	// バッファサイズ
	private int READ_BUF_SIZE = 1;

	/**
	 * レスポンス作成
	 */
	public void makeResponse(EIMCommandResult resultData, HttpServletRequest request, HttpServletResponse response) throws Exception
	{
		// レスポンスヘッダー設定
		setResponseHeader(response, resultData);
		
		// ファイルの中身をバイナリ形式でレスポンスにセットする
		outputFileDataToResponse(resultData, response);
	}
	
	/**
	 * レスポンスヘッダにファイル名称を設定
	 * @param response
	 * @throws Exception
	 */
	protected void setResponseHeader(HttpServletResponse response, EIMCommandResult resultData) throws  Exception
	{
		response.setContentType("application/octet-stream; charset=utf-8");
		response.setHeader("Content-Disposition", "attachment; filename=\"" + getHeaderFileName(resultData) + "\"");
	}
	
	/**
	 * レスポンスヘッダにセットするファイル名称を取得
	 * @param resultData
	 * @return
	 * @throws Exception
	 */
	abstract protected String getHeaderFileName(EIMCommandResult resultData) throws Exception;
	
	/**
	 * ファイルの中身をバイナリ形式でレスポンスにセットする
	 * @param resultData
	 * @param response
	 * @throws Exception
	 */
	protected void outputFileDataToResponse(EIMCommandResult resultData, HttpServletResponse response) throws Exception
	{
		//OutputStream
		OutputStream out = response.getOutputStream();
		FileInputStream in = new FileInputStream(getFileObj(resultData));
		
		byte[] readBytes = new byte[READ_BUF_SIZE];
		int buff = 0;
		while((buff = in.read(readBytes)) != -1)
		{
			out.write(readBytes, 0, buff);
		}
		in.close();
		out.flush();
		out.close();
	}
	
	/**
	 * レスポンスにセットするファイルオブジェクトを取得する
	 * @param resultData
	 * @return
	 * @throws Exception
	 */
	abstract protected File getFileObj(EIMCommandResult resultData) throws Exception;
	
}
