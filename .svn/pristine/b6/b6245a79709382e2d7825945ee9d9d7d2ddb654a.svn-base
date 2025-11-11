package common.util;

import java.util.ArrayList;
import java.util.List;

import jakarta.servlet.http.HttpServletRequest;

import eim.bo.EIMAttributeType;
import eim.bo.EIMComp;
import eim.bo.EIMException;
import eim.bo.EIMGroup;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRole;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.CompUtils;
import eim.util.EIMConfig;
import eim.util.EIMXmlConfigAdminAuth;
import eim.util.GroupUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RoleUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.TypeConvertUtils;
import eim.util.UserUtils;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;

/**
 * アプリケーション共通ワークスペース関連処理クラス
 */

public class WorkSpaceUtil {
	
	//絞りこみフラグ
	private static int NOT_LIMIT_FLAG = 0;
	private static int IS_LIMIT_FLAG = 1;
	
	
	/**
	 * ワークスペース管理に必要な属性情報を更新します。
	 * 設定する項目は以下の通り。
	 * 
	 * <li>管理者（責任者）
	 * <li>使用可能タイプ（ドキュメント/フォルダー/タグ）
	 * <li>使用可能セキュリティ
	 * 
	 * @param sess EIMSession
	 * @param request 属性更新情報の管理クラス
	 * @param object 属性情報設定対象のワークスペースオブジェクト
	 * @throws Exception
	 */
	public static void updateAttributeForWSManagement(EIMSession sess, HttpServletRequest request, EIMObject object) throws Exception {
				
				//1)責任者
				//Parameter
				String[] prmAdminIds = request.getParameterValues("entryList");
				String[] prmAdminTypeIds = request.getParameterValues("entryTypeList");
				
				if((prmAdminIds != null && prmAdminIds.length != 0 && !prmAdminIds[0].equals(""))
						&& (prmAdminTypeIds != null && prmAdminTypeIds.length != 0 && !prmAdminTypeIds.equals("")))
				{
					long[] adminIntIds = getSelectedObjIds(prmAdminIds);
					long[] adminTypeIntIds = getSelectedObjIds(prmAdminTypeIds);
					
					checkWorkspaceAdminEntryExist(sess, adminIntIds, adminTypeIntIds);
					
					ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR"))
							, TypeConvertUtils.convertToBuildTypeArray(adminIntIds));
					ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR_TYPE"))
							, TypeConvertUtils.convertToBuildTypeArray(adminTypeIntIds));
				}else{
					ObjectAttributeUtils.deleteAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR")));
					ObjectAttributeUtils.deleteAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR_TYPE")));
				}
				
				
				//2)使用可能タイプ
				//Parameter
				String prmDocAllFlag = request.getParameter("docAllFlag");
				String prmFolderAllFlag = request.getParameter("folderAllFlag");
				String prmTagAllFlag = request.getParameter("tagAllFlag");
				
				String[] prmSelectableDoc = request.getParameterValues("docList");
				String[] prmSelectableFolder = request.getParameterValues("folderList");
				String[] prmSelectableTag = request.getParameterValues("tagList");
				
				checkAllUsableTypeExist(sess, prmSelectableDoc, prmSelectableFolder, prmSelectableTag);
				
				//各種絞込みフラグ設定
				int isLimitDoc = getLimitFlagInt(prmDocAllFlag);
				ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_DOCUMENT_TYPE_FLAG")), isLimitDoc);
				
				int isLimitFolder = getLimitFlagInt(prmFolderAllFlag);
				ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_FOLDER_TYPE_FLAG")), isLimitFolder);
				
				int isLimitTag = getLimitFlagInt(prmTagAllFlag);
				ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_TAG_TYPE_FLAG")), isLimitTag);
				
				
				//各種使用可能ID設定
				//ドキュメント
				if(prmSelectableDoc == null){
					ObjectAttributeUtils.deleteAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_DOCUMENT_TYPE")));
				}else{
					long[] selectableDocIds =getSelectedObjIds(prmSelectableDoc);
					ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_DOCUMENT_TYPE"))
							, TypeConvertUtils.convertToBuildTypeArray(selectableDocIds));
				}
				
				//フォルダ
				if(prmSelectableFolder == null){
					ObjectAttributeUtils.deleteAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_FOLDER_TYPE")));
				}else{
					long[] selectableForderIds = getSelectedObjIds(prmSelectableFolder);
					ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_FOLDER_TYPE"))
							, TypeConvertUtils.convertToBuildTypeArray(selectableForderIds));
				}
				
				//タグ
				if(prmSelectableTag == null){
					ObjectAttributeUtils.deleteAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_TAG_TYPE")));
				}else{
					long[] selectableTagIds = getSelectedObjIds(prmSelectableTag);
					ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_TAG_TYPE"))
							, TypeConvertUtils.convertToBuildTypeArray(selectableTagIds));
				}
				
				//3)使用可能セキュリティ
				//Parameter
				String prmSecAllFlag = request.getParameter("secAllFlag");
				String[] prmSelectableSec = request.getParameterValues("secList");
				
				int isLimitSec = getLimitFlagInt(prmSecAllFlag);
				ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_SECURITY_FLAG")), isLimitSec);
				
				//使用可能SecID
				if(prmSelectableSec == null){
					ObjectAttributeUtils.deleteAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_SECURITY")));
				}else{
					long[] selectableSecIds = getSelectedObjIds(prmSelectableSec);
					checkUsableSecurityExist(sess, selectableSecIds);
					ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_SECURITY"))
							, TypeConvertUtils.convertToBuildTypeArray(selectableSecIds));
				}

				// 手動削除禁止フラグ
				int isManualDeleteFlag = 0;
				String prmIsManualDeleteFlag = request.getParameter("isManualDeleteFlag");
				if (prmIsManualDeleteFlag != null && prmIsManualDeleteFlag.equals("true")) {
					isManualDeleteFlag = 1;
				}
				ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_MANUAL_DELETE_PROHIBITED_FLAG")), isManualDeleteFlag);

	}
	
	/**
	 * 使用可能制限フラグ属性に設定する値を返却します。
	 * 
	 * @param prmFlag 「全て」フラグ（"true"…「全てを適用する」/ "false"…「制限する」）
	 * @return isLimitFlag 属性に設定するint値（0…「制限しない（全て）」/1…「制限する」
	 */
	private static int getLimitFlagInt(String prmFlag){
		
		int isLimitFlag = 0;
		
		if(prmFlag != null){
			isLimitFlag = (prmFlag.equals("true")) ? NOT_LIMIT_FLAG : IS_LIMIT_FLAG;
		}
		return isLimitFlag;
	}
	
	/**
	 * 使用可能タイプ/セキュリティ属性に設定する値(IDのint配列)を返却します。
	 * 
	 * @param strIds 使用可能なオブジェクトのIDが入ったString配列
	 * @return selectedIds 属性に設定するint配列
	 */
	private static long[] getSelectedObjIds(String[] strIds){
		long[] selectedIds = new long[strIds.length];
		for(int i=0; i < strIds.length; i++)
		{
			selectedIds[i] = Long.parseLong(strIds[i]);
		}
		return selectedIds;
	}
	
	
	/**
	 * 
	 * 指定されたユーザがワークスペースの責任者であるかどうかチェックします。
	 * 
	 * @param user EIMUser
	 * @param obj ワークスペースオブジェクト
	 * @return 責任者であればtrue
	 * @throws Exception
	 */
	public static boolean isWorkSpaceAdminUser(EIMSession sess, EIMUser user, EIMObject object) throws Exception{
		
		long[] adminIds = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR"));
		if(adminIds != null){
			
			// 「責任者種別」属性を取得
			long[] adminTypeIds = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR_TYPE"));
			if(adminTypeIds != null){
				
				// ユーザが所属するグループ、ロールを取得
				List<EIMGroup> belongGroupList = GroupUtils.getGroupByUser(sess, user);
				List<EIMRole> belongRoleList = RoleUtils.getRoleByUser(sess, user);
				
				for(int i=0; i < adminTypeIds.length; i++){
					
					long type = adminTypeIds[i];	//責任者種別
					long adminId = adminIds[i];	//責任者ID
					
					if(type == AppConstant.WS_ADMIN_ENTRY_TYPE_USER){
						if(adminId == user.getId()){
							return true;
						}
					}
					else if(type == AppConstant.WS_ADMIN_ENTRY_TYPE_GROUP){
						for(EIMGroup group : belongGroupList){
							if(group.getId() == adminId){
								return true;
							}
						}
					}
					else if(type == AppConstant.WS_ADMIN_ENTRY_TYPE_ROLE){
						for(EIMRole role : belongRoleList){
							if(role.getId() == adminId){
								return true;
							}
						}
					}
					else if(type == AppConstant.WS_ADMIN_ENTRY_TYPE_COMP_GROUP){
						EIMComp comp = CompUtils.getCompById(sess, adminId);
						if(comp != null){
							List<EIMUser> userList = CompUtils.getUserList(sess, comp);
							for(EIMUser compUser : userList){
								if(compUser.getId() == user.getId()){
									return true;
								}
							}
						}
					}
				}
			}
		}else{
			// 責任者属性が存在しない
			return false;
		}
		
		return false;
	}
	
	
	/**
	 * ユーザが「ワークスペース作成」権限を持つか、ワークスペースの責任者に設定されていれるか
	 * どうか判定します。
	 * 
	 * @param sess EIMSession
	 * @param user EIMUser
	 * @param object 対象となるワークスペースオブジェクト
	 * @return 権限があれば true 、さもなくば false
	 * @throws Exception
	 */
	public static boolean hasAnyWorkSpaceAdminAuth(EIMSession sess, EIMUser user, EIMObject object) throws Exception{
		
		// ワークスペース作成権限を持つ
		if(EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, AppConstant.ADMIN_AUTH_ID_WORKSPACE)){
			return true;
		}
		// ワークスペースの責任者である
		if(object != null){
			if(isWorkSpaceAdminUser(sess, user, object)){
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * ワークスペース責任者に設定されたエントリーの存在チェックを行います。
	 * adminIdsとadminTypeIdsの長さが同じであることが前提です。
	 * @param sess セッション情報
	 * @param adminIds 責任者に設定されたユーザ・グループ・ロール・複合グループのIDの配列
	 * @param adminTypeIds ユーザ・グループ・ロール・複合グループのタイプを示す配列
	 * @exception EIMAppException 配列内に存在しないIDが含まれる場合
	 */
	private static void checkWorkspaceAdminEntryExist(EIMSession sess, long[] adminIds, long[] adminTypeIds)throws Exception{
		for(int i = 0; i < adminIds.length; i++){
			if(AppConstant.WS_ADMIN_ENTRY_TYPE_USER == adminTypeIds[i]){
				// ユーザ
				EIMUser user = UserUtils.getUserById(sess, adminIds[i]);
				if(user == null){
					throw new EIMAppException(sess, "EIM.ERROR.LOGIC.WORKSPACE.ADMIN.INCOLLECT");
				}
			}else if(AppConstant.WS_ADMIN_ENTRY_TYPE_GROUP == adminTypeIds[i]){
				// グループ
				EIMGroup group = GroupUtils.getGroupById(sess, adminIds[i]);
				if(group == null){
					throw new EIMAppException(sess, "EIM.ERROR.LOGIC.WORKSPACE.ADMIN.INCOLLECT");
				}
			}else if(AppConstant.WS_ADMIN_ENTRY_TYPE_ROLE == adminTypeIds[i]){
				// ロール
				EIMRole role = RoleUtils.getRoleById(sess, adminIds[i]);
				if(role == null){
					throw new EIMAppException(sess, "EIM.ERROR.LOGIC.WORKSPACE.ADMIN.INCOLLECT");
				}
			}else if(AppConstant.WS_ADMIN_ENTRY_TYPE_COMP_GROUP == adminTypeIds[i]){
				// 複合グループ
				EIMComp comp = CompUtils.getCompById(sess, adminIds[i]);
				if(comp == null){
					throw new EIMAppException(sess, "EIM.ERROR.LOGIC.WORKSPACE.ADMIN.INCOLLECT");
				}
			}
		}
	}
	
	/**
	 * 使用可能セキュリティに設定されたセキュリティの存在チェックを行います。
	 * @param sess セッション情報
	 * @param securityIds セキュリティIDの文字列の配列
	 * @throws EIMAppException 配列内に存在しないIDが含まれる場合
	 */
	private static void checkUsableSecurityExist(EIMSession sess, long[] securityIds)throws Exception{
		for(int i = 0; i < securityIds.length; i++){
			EIMSecurity sec = SecurityUtils.getSecurityById(sess, securityIds[i]);
			if(sec == null){
				throw new EIMAppException(sess, "EIM.ERROR.LOGIC.WORKSPACE.USABLE.SECURITY.INCOLLECT");
			}
		}
	}
	
	/**
	 * 使用可能タイプ（ドキュメント、フォルダ、タグ）の存在チェックを一括で行います。
	 * @param sess セッション情報
	 * @param usableDocTypes 使用可能ドキュメントタイプの配列
	 * @param usableFolderTypes 使用可能フォルダタイプの配列
	 * @param usableTagTypes 使用可能タグタイプの配列
	 * @throws Exception
	 */
	private static void checkAllUsableTypeExist(EIMSession sess, String[] usableDocTypes, String[] usableFolderTypes, String[] usableTagTypes)throws Exception{
		List<String> typeIdStringList = new ArrayList<String>();
		if(usableDocTypes != null){
			for(int i = 0; i < usableDocTypes.length; i++){
				typeIdStringList.add(usableDocTypes[i]);
			}
		}
		
		if(usableFolderTypes != null){
			for(int i = 0; i < usableFolderTypes.length; i++){
				typeIdStringList.add(usableFolderTypes[i]);
			}
		}
		
		if(usableTagTypes != null){
			for(int i = 0; i < usableTagTypes.length; i++){
				typeIdStringList.add(usableTagTypes[i]);
			}
		}
		
		if(typeIdStringList.size() > 0){
			checkUsableTypeExist(sess, typeIdStringList.toArray(new String[typeIdStringList.size()]));
		}
	}
	
	/**
	 * 使用可能オブジェクトタイプに設定されたオブジェクトタイプの存在チェックを行います。
	 * @param sess セッション情報
	 * @param typeIds オブジェクトタイプのID文字列の配列
	 * @throws EIMAppException 指定されたオブジェクトタイプIDが1件でもヒットしなかった場合
	 */
	private static void checkUsableTypeExist(EIMSession sess, String[] typeIds)throws Exception{
		int hitCount = 0;
		
		// オブジェクトタイプ「オブジェクトタイプ」
		EIMObjectType objTypeObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.getValue("OBJECT_TYPE_NAME_OBJECTTYPE"));
		
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		
		// オブジェクト検索ヘルパー
		EIMSearchSelectEIMObject.SearchConditionBuildHelper helper = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		
		selectTarget.setCondition(helper.group(helper.opAnd())
			.addCondition(
				// 検索条件1:名称が指定された配列
				helper.in(helper.opAnd(), PsedoAttributeTypeEnum.NAME, helper.opIn(), typeIds))
			.addCondition(
				// 検索条件2:オブジェクトタイプが「オブジェクトタイプ」
				helper.eq(helper.opAnd(), PsedoAttributeTypeEnum.TYPE, objTypeObjType.getId())));
		
		// 返却条件を指定
		List<EIMAttributeType> resultAttrList = new ArrayList<EIMAttributeType>();
		// 件数だけ知りたいのでIDだけでよい
		resultAttrList.add(PsedoAttributeTypeEnum.ID);
		selectTarget.setResultAttrs(resultAttrList);
		
		// 上限件数を指定
		EIMSearchLimitCountCondition limitCond = new EIMSearchLimitCountCondition(0, true);
		
		try{
			SearchUtils.searchObjects(sess, selectTarget, limitCond);
		}catch(EIMException e){
			// ヒット件数取得
			hitCount = (Integer)e.getMessageParams()[0];
		}
		
		// 検索条件の配列の長さとヒット件数が一致しなければエラー
		if(hitCount != typeIds.length){
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.WORKSPACE.USABLE.TYPE.INCOLLECT");
		}
	}
}
