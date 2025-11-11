package common.util;

import eim.bo.EIMException;
import eim.bo.EIMResource;
import eim.util.StringUtils;

/**
 * エラールーチン　共通クラス
 * <P>
 * [機能]<BR>
 * 
 * [備考]<BR>
 *
 * @author tsumura
 * @version 1.0
 */
public class AppMessageUtils {

	/**
	 * エラーメッセージ作成処理<BR>
	 * 
	 * @param exception Exceptionオブジェクト
	 * @return レスポンス用エラータグ(XML)文字列
	 */
	public static String makeErrorMessage(Exception exception){
		return makeErrorMessage(exception, null);
	}
	
	/**
	 * エラーメッセージ作成処理<BR>
	 * 
	 * @param exception Exceptionオブジェクト
	 * @param param メッセージのバインド値(必要ない場合はNull)
	 * @return レスポンス用エラータグ(XML)文字列
	 */
	public static String makeErrorMessage(Exception exception, Object[] param){
		
		StringBuffer errBuf = new StringBuffer();
		
		/*
		 * Exceptionオブジェクト種類別処理
		 */
		String errMsg = null;
		if(exception instanceof EIMException){
			EIMException eime = (EIMException)exception;

			if(param == null){
				errMsg = EIMResource.getMessage(EIMResource.getKey(eime.getCode()));
			}
			else{
				// バインドする値が存在する場合
				errMsg = EIMResource.getMessage(EIMResource.getKey(eime.getCode()), param);
			}
		}
		else{
			// EIMException
			errMsg = exception.getMessage();
			errBuf.append(errMsg);
		}
		
		return errBuf.toString();
	}

	/**
	 * ログメッセージ作成処理<BR>
	 * 
	 * @param message メッセージ
	 * @return ログ出力文字列
	 */
	public static String makeLogMessage(String message){
		return makeLogMessage(0, message, null);
	}

	/**
	 * ログメッセージ作成処理<BR>
	 * 
	 * @param usrId ユーザID
	 * @param message メッセージ
	 * @return ログ出力文字列
	 */
	public static String makeLogMessage(long usrId, String message){
		return makeLogMessage(usrId, message, null);
	}
	/**
	 * ログメッセージ作成処理<BR>
	 * 
	 * @param usrId ユーザID
	 * @param message メッセージ
	 * @param param メッセージの付加値(キー値)配列
	 * @return ログ出力文字列
	 */
	public static String makeLogMessage(long usrId, String message, Object[] param){
		
		StringBuffer msgBuf = new StringBuffer();
		
		// ユーザIDの設定
		if(usrId == 0){
			msgBuf.append("null");
		}
		else{
			msgBuf.append(usrId);
		}
		msgBuf.append(" ");
		msgBuf.append(message);

		/*
		 * 付加値設定
		 */
		if(param == null){
			return msgBuf.toString();
		}

		for(int i=0; i<param.length; i++){
			msgBuf.append("(");
			msgBuf.append(param[i]);
			msgBuf.append(")");
		}
		return msgBuf.toString();
	}
	
	/**
	 * エラータグ作成処理<BR>
	 * 
	 * @param errMesage エラータグに設定するエラーメッセージ
	 * @return レスポンス用エラータグ(XML)文字列
	 */
	public static String makeErrorTagByMessage(String errMesage){

		StringBuffer errBuf = new StringBuffer();
		errBuf.append("<error");
		errBuf.append(" message=\"" + StringUtils.xmlEncode(errMesage) + "\"");
		errBuf.append(">");
		errBuf.append("</error>");
		return errBuf.toString();
	}
}