package common.bo;

import eim.bo.EIMObject;
import eim.bo.EIMResource;

/**
 * メッセージ格納クラス
 * 
 * <li>メール等で複数メッセージをまとめて出力する際に利用します。
 *
 */
public class MessageItem {
	
	/**
	 * メッセージ・キー
	 */
	private String _messageKey = null;
	
	/**
	 * メッセージ・パラメータ
	 */
	private String [] _params = null;
	
	/**
	 * メッセージ対象のEIMObject
	 * 
	 * <li>メッセージ内容には影響与えません。単なる保管・参照用。
	 */
	private EIMObject _eimObject = null;
	
	/**
	 * コンストラクタ
	 * 
	 * @param messageKey メッセージ・キー
	 * @param params メッセージ・パラメータ
	 */
	public MessageItem(String messageKey, String[] params) {
		this._messageKey = messageKey;
		this._params = params;
	}

	/**
	 * コンストラクタ
	 * 
	 * @param messageKey メッセージ・キー
	 * @param params メッセージ・パラメータ
	 * @param obj メッセージ対象のEIMObject
	 */
	public MessageItem(String messageKey, String[] params, EIMObject obj) {
		this._messageKey = messageKey;
		this._params = params;
		this._eimObject = obj;
	}
	
	/**
	 * 引数で指定された言語のメッセージを返します。
	 * 
	 * @param langId 言語ID
	 * @return 引数で指定された言語のメッセージ
	 */
	public String getMessage(String langId) {
		if (_params != null) {
			return EIMResource.getMessage(langId, _messageKey, _params);
		} else {
			return EIMResource.getMessage(langId, _messageKey);
		}
	}
	
	/**
	 * ログ出力言語(config.propertiesの「MESSAGELANG」)のメッセージを返します。
	 * 
	 * @return ログ出力言語のメッセージ
	 */
	public String getMessage() {
		if (_params != null) {
			return EIMResource.getMessage(_messageKey, _params);
		} else {
			return EIMResource.getMessage(_messageKey);
		}
	}
	
	/**
	 * メッセージ対象のEIMObjectを返します。
	 * 
	 * @return メッセージ対象のEIMObject
	 */
	public EIMObject getEimObject() {
		return _eimObject;
	}
}