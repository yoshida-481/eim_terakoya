package common.util;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;

import common.bo.AttributeUpdater;
import common.bo.AttributeUpdaterItem;

import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.net.EIMSession;
import eim.util.EIMUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;

/**
 * 
 * 属性値更新時のヘルパークラス
 *
 */
public class UpdateAttributeHelper {
	
	/**
	 * 属性情報画面で入力された属性情報を更新します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param request 属性更新情報の管理クラス
	 * @param object 属性情報設定対象のオブジェクト
	 * @param isUpdateOnlyName オブジェクト名の更新のみの場合は true を設定。他の属性も更新する場合はfalseを設定。
	 * @throws Exception
	 */
	public static void updateAttribute(EIMSession sess, HttpServletRequest request, EIMObject object, boolean isUpdateOnlyName) throws Exception {
		
		//Parameter
		String prmObjTypeId = EIMUtils.getParameter(request, "objTypeId");
		String prmObjName = EIMUtils.getParameter(request, "objName");
		String prmAttTypeNameAllocate = EIMUtils.getParameter(request, "attType_nameAllocate");

		List attUpdateItemList = null;
		
		if ( ! isUpdateOnlyName) {
			
			attUpdateItemList = new ArrayList();
			
			// [09/03/04 deleted by ik.] 
			// リクエストパラメータから属性タイプIDを抽出するように変更したことに伴いコメントアウト
			//Object Type
			/*
			EIMObjectType objType = null;
			if (prmObjTypeId != null && prmObjTypeId.length() > 0) {

				objType = ObjectUtils.getObjectTypeById(sess, Integer.parseInt(prmObjTypeId));
			
			}
			else {

				objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());

			}
			*/
			
			// [09/03/04 modified by ik.]
			// 更新対象属性の属性タイプIDのリストを HTTP リクエストパラメータから抽出するように変更
			Hashtable attTypeIdList = (Hashtable)getAttTypeIdListByReqParam(request);
			long attTypeId;
			/* 
			//Attribute Types
			List attTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
			//Attribute Type
			EIMAttributeType attType = null;
			*/
			
			//Update User Attributes
			Enumeration keys = attTypeIdList.keys();
			while (keys.hasMoreElements()) {
				attTypeId = Long.parseLong((String)keys.nextElement());
				
				// [09/03/04 deleted by ik.] 
				// リクエストパラメータから属性タイプIDを抽出するように変更したことに伴いコメントアウト
				//Skip System Attributes
				/*
				if(attType.getDefName().equals("パス") ||
						attType.getDefName().equals("作成者") ||
						attType.getDefName().equals("作成日") ||
						attType.getDefName().equals("更新者") ||
						attType.getDefName().equals("更新日") ||
						attType.getDefName().equals("前セキュリティ") ||
						attType.getDefName().equals("上位からの引継ぎ属性") ||
						attType.getDefName().equals("上位WFフォルダ") ||
						attType.getDefName().equals("公開処理失敗") ||
						attType.getDefName().equals("名称割当て属性") ||
						attType.getDefName().equals("下位への引継ぎ属性") ||
						attType.getDefName().equals("下位フォルダ管理セキュリティ") ||
						attType.getDefName().equals("サイズ") ||
						attType.getDefName().equals("PDF結合処理失敗") ||
						attType.getDefName().equals("PDF分割状態") ||
						attType.getDefName().equals("リンク先") ||
						attType.getDefName().equals("ドキュメントリンク") ||
						attType.getDefName().equals("タグ") ||
						attType.getDefName().equals("タグ付与者") ||
						attType.getDefName().equals("タグ付与日") ||
						attType.getDefName().equals("署名・暗号化状態") ||
						attType.getDefName().equals("署名・暗号化バージョン") )
				{
					continue;
				}
				*/
				
				String isReadOnly = EIMUtils.getParameter(request, "attType_" + attTypeId + "_readOnly");
				if (isReadOnly != null && isReadOnly.equals("true")) {

					continue;

				}
				
				List valueList = new ArrayList();
				int pos = 0;
				
				while(true) {
					String param = EIMUtils.getParameter(request, "attType_" + attTypeId + "_" + pos);
					if (param == null || param.length() <= 0) {

						break;

					}

					valueList.add(param);

					pos++;
				}
				
				String[] attValues = null;
				if (valueList.size() > 0) {

					attValues = (String[])valueList.toArray(new String[valueList.size()]);

				}

				
				// 名称割当て属性
				boolean isNameAllocate = false;

				if (prmAttTypeNameAllocate != null && prmAttTypeNameAllocate.length() > 0) {
					// 名称割当て判定
					if (Long.parseLong(prmAttTypeNameAllocate) == attTypeId) {
						isNameAllocate = true;
					}
				}

				// 下位引継ぎ属性
				boolean isLowerSuccession = false;
				String prmLowerSuccession = EIMUtils.getParameter(request, "attType_" + attTypeId + "_lowerSuccession");			

				if (prmLowerSuccession != null
					&& prmLowerSuccession.equals("1")) {

						isLowerSuccession = true;

				}
							
				AttributeUpdaterItem attUpdateItem = new AttributeUpdaterItem(attTypeId,
																			   attValues, 
																			   isNameAllocate,
																			   isLowerSuccession);
				
				attUpdateItemList.add(attUpdateItem);
			}
		}
		
		AttributeUpdater attUpdater = new AttributeUpdater(object.getId(), prmObjName, attUpdateItemList);
				
		//属性値更新処理
		AttributeUtil.updateAttribute(sess, attUpdater);
		
	}
	
	/** [09/03/04 added by ik.]
	 *  HTTP リクエストパラメータから更新対象属性の属性タイプIDのリストを抽出する。
	 * @param request HTTP リクエストパラメータ
	 * @return request から抽出した更新対象属性タイプIDのリスト
	 */
	private static Map getAttTypeIdListByReqParam(HttpServletRequest request) {
		Hashtable idList = new Hashtable();
		String paramName = null;
		
		// パラメータ名から属性タイプIDを抽出
		// --> attType_xxx_*** から「xxx」の部分を抽出
		// --> attType_nameAllocate 属性は外す。
		Enumeration e = request.getParameterNames();
		while (e.hasMoreElements()) {
			paramName = (String)e.nextElement();
			if (!paramName.startsWith("attType_")) {
				continue;
			}
			String[] splitedStrs = paramName.split("_");
			if (splitedStrs == null || splitedStrs.length != 3) {
				continue;
			}
			idList.put(splitedStrs[1], "");
		}
		return idList;
	}
}