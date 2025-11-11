package eim.command.common.util;

import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;

public class EIMCommandObjectUtil {

	/**
	 * オブジェクトリンクを保持するか
	 * @param object
	 * @return
	 */
	public static boolean hasObjectLink(EIMObject object) {
		boolean result = false;
		
		// (オブジェクトタイプがドキュメントで、且つ属性「ドキュメントリンク」が「1」の場合)
		if (object != null 
				&& object.getType().getName().equals(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"))
				&& (object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK")) != null)) {
			result = true;
		}
		return result;
	}

	/**
	 * ルートのオブジェクトタイプを取得する
	 * @param sess
	 * @param id 対象となるオブジェクトID
	 * @return
	 * @throws Exception
	 */
	public static EIMObjectType getRootObjType(EIMSession sess, long id) throws Exception{
		
		EIMObject object = ObjectUtils.getObjectById(sess, id);
		if (object != null) {
			return EIMCommandObjectUtil.getRootObjType(object.getType());
		}
		return null;
	}
	private static EIMObjectType getRootObjType(EIMObjectType childType) throws Exception{
		EIMObjectType parentType = childType.getParent();
		if(parentType != null){
			childType = getRootObjType(parentType);
		}
		return childType;
	}

}
