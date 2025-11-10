package common.util;

import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMObject;
import eim.bo.EIMUser;

/**
 * ScheduledMailDeliverクラス
 * 
 * <li>メールをユーザ単位で送付する(複数ドキュメントを1メールにまとめる)ために本クラスを利用します。
 * 
 */
public class ScheduledMailDeliver
{
	private EIMUser _user = null;
	private List _objList = new ArrayList();
	
	/**
	 * コンストラクタ
	 * 
	 * @param user ユーザ
	 * @param object EIMオブジェクト
	 */
	public ScheduledMailDeliver(EIMUser user, EIMObject object)
	{
		_user = user;
		_objList.add(object);
	}
	
	/**
	 * コンストラクタ（ユーザを設定します）
	 * 
	 * @param user ユーザ
	 */
	public ScheduledMailDeliver(EIMUser user)
	{
		_user = user;
	}
	
	/**
	 * ユーザを返却します。
	 * 
	 * @return ユーザ
	 */
	public EIMUser getUser()
	{
		return _user;
	}
	
	/**
	 * EIMオブジェクトをリストに追加します。
	 * 
	 * @param object EIMオブジェクト
	 */
	public void add(EIMObject object)
	{
		_objList.add(object);
	}
	
	/**
	 * EIMオブジェクトのリストを取得します。
	 * 
	 * @return　EIMオブジェクトのリスト
	 */
	public List getObjectList()
	{
		return _objList;
	}
}