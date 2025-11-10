package jp.co.ctc_g.eim.app.document.common.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import jp.co.ctc_g.eim.framework2.business.dao.ObjectDao;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.SecurityService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * 【ドキュメントAPI】
 * 属性情報更新処理の共通クラス
 *
 */
public class AttributeUtil {

	/** [起動モード] 属性情報更新 */
	private static final int MODE_UPDATE = 0;

	/** [起動モード] 移動 (カット＆ペースト) */
	private static final int MODE_MOVE = 1;

	/** [起動モード] コピー＆ペースト */
	private static final int MODE_COPY = 2;

	/** [起動モード] 論理削除(ごみ箱へ移動) */
	private static final int MODE_DELETE = 3;

	/** [起動モード] 論理削除の解除(ごみ箱からの移動) */
	private static final int MODE_RETURN = 4;

	/**
	 * 「論理削除」で移動するObjectDomainの属性情報を更新します。
	 *
	 * <li>対象ObjectDomainの上位引継ぎ属性に該当する属性を、対象以下の全ObjectDomainから削除します。
	 *    (ObjectTypeDomainの違いにより途中で分断されている場合は属性の削除もその階層で止まる)
	 * <li>対象以下の全ObjectDomainから上位WFフォルダを更新します。
	 * <li>対象ObjectDomainと削除元の親ObjectDomainのセキュリティが異なる場合、前セキュリティを保存します。
	 *
	 * @param obj 論理削除する対象のObjectDomain
	 * @param parentObj 対象の親ObjectDomain
	 * @throws Exception
	 */
	public static void updateAttributeForDelete(ObjectDomain obj, ObjectDomain parentObj) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService2");
		SecurityService securityService = (SecurityService) context.getBean("securityService2");

		checkStatus(obj, parentObj);

		// 元々の上位引継ぎ属性の取得 → 削除すべき上位引継ぎ属性
		List<Long> delAttrs = AppDocumentUtil.getIntAttrs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_FROM_HIGH_ATTR")); // 上位からの引継ぎ属性

		// システムごみ箱オブジェクトの取得
		ObjectTypeDomain recycleObjType = AppDocumentLogicUtil.getRecycleObjectType();
		ObjectDomain recycleObj = objectService.getByTypeAndName(recycleObjType, ConfigUtils.getByKey("OBJECT_TYPE_NAME_RECYCLE"));

		// 削除対象オブジェクトからパスを取得し、ワークスペース固有ごみ箱自体のパスを取得
		String objPath = obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getString();
		String [] strs = objPath.split("/");
		String wsRecycleObjPath = "/" + strs[1] + "/" ;
		long secId;

		if (!AppDocumentUtil.isObjectInWsRecycle(obj)) {
			// 「ワークスペース固有ごみ箱配下セキュリティ」の取得
			secId = (securityService.getByDefinitionName(ConfigUtils.getByKey("SECURITY_NAME_WORKSPACE_RECYCLE_CONTENTS"))).getId();
			// 「パス」属性をワークスペース固有ごみ箱配下のパスに変更
			objPath = wsRecycleObjPath + recycleObj.getName() + "/";
		} else {
			// 「system」セキュリティの取得
			secId = securityService.getByDefinitionName(ConfigUtils.getByKey("SECURITY_NAME_SYSTEM")).getId();
			// 「パス」属性をシステムごみ箱配下のパスに変更
			objPath = "/" + recycleObj.getName() + "/";
		}
		// 自分および自分以下のオブジェクトに削除すべき上位引継ぎ属性の除去、上位WFフォルダの更新、前セキュリティの更新 (再帰処理)
		updateObjAttr(obj, MODE_DELETE, objPath, new HashMap<Long, Object>(), delAttrs, 0, null, secId, null);


	}

	/**
	 * 「切り取り＆貼付け」で移動するObjectDomainの属性情報を更新します。
	 *
	 * <li>対象ObjectDomainの上位引継ぎ属性に該当する属性を、対象以下の全ObjectDomainから削除します。
	 *    (ObjectTypeDomainの違いにより途中で分断されている場合は属性の削除もその階層で止まる)
	 * <li>貼付け先ObjectDomainの下位引継ぎ属性で、対象以下の全ObjectDomainを更新します。
	 * <li>対象以下の全ObjectDomainから上位WFフォルダを更新します。
	 * <li>各オブジェクトの論理削除前のセキュリティを復元します。ただしDBに既に存在しないセキュリティの場合は、systemセキュリティを適用します。
	 *
	 * @param obj 移動する対象のObjectDomain
	 * @param nextParentObj 貼付け先のObjectDomain
	 * @param relType リレーションタイプ
	 * @param path 設定すべきパス
	 * @param inRecycleBox 切り取り対象がごみ箱の中か否か (true:ごみ箱の中/false:ごみ箱の外)
	 * @throws Exception
	 */
	public static void updateAttributeForMove(ObjectDomain obj, ObjectDomain nextParentObj, RelationTypeDomain relType, String path, boolean inRecycleBox) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");
		SecurityService securityService = (SecurityService) context.getBean("securityService2");

		/* ステータスのチェック */

		// 貼付け先のステータス種別ID
		long stsKind = nextParentObj.getStatus() != null ? nextParentObj.getStatus().getType().getBase().getId()
				: AppConstant.STATUS_TYPE_KIND_ID_NONE;

		// 貼付け先の上位WFフォルダのステータスのチェック
		if ((AppDocumentLogicUtil.isTypeOfFolderWithWorkflow(nextParentObj) || AppDocumentLogicUtil.isTypeOfFolderUnderFolderWithWorkflow(nextParentObj))
				&& stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
			// 切り取り/貼付け権限がありません。
			throw new EIMException("EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
		}

		// 親リレーションの取得
		List<RelationDomain> parentRelList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relType, obj, new AccessRoleTypeDomain("READ"));

		// 切り取り元フォルダの取得
		ObjectDomain baseParentObj = null;
		if (parentRelList != null && parentRelList.size() > 0) {
			baseParentObj = parentRelList.get(0).getParent();
			if (baseParentObj != null) {

				// 切り取り元フォルダの上位WFフォルダのステータスのチェック
				if ((AppDocumentLogicUtil.isTypeOfFolderWithWorkflow(baseParentObj) || AppDocumentLogicUtil.isTypeOfFolderUnderFolderWithWorkflow(baseParentObj))
						&& (baseParentObj.getStatus() != null && baseParentObj.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING)) { // 上位WFのステータスが「編集中」以外の場合はエラー
					// 切り取り/貼付け権限がありません。
					throw new EIMException("EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
				}
			}
		}

		// ごみ箱内ではない場合、削除すべき上位引継ぎ属性の取得
		List<Long> delAttrs = null;
		if (!inRecycleBox) {
			// 元々の上位引継ぎ属性の取得 → 削除すべき上位引継ぎ属性
			delAttrs = AppDocumentUtil.getIntAttrs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_FROM_HIGH_ATTR")); // 上位からの引継ぎ属性
		}

		// 下位に引継ぐ属性値のMap [key]属性タイプID [value]属性の値(配列)
		HashMap<Long, Object> setLowAttrMap = new HashMap<Long, Object>();

		// 貼付け先オブジェクトのオブジェクトタイプに設定されている属性タイプ一覧を取得
		AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
		attributeTypeCriteria.setObjectTypeId(nextParentObj.getType().getId());
		// 対象オブジェクトのオブジェクトタイプが持つ属性タイプを取得
		List<AttributeTypeDomain> attTypeList = attributeTypeService.getList(attributeTypeCriteria);

		// 貼付け先オブジェクトの属性タイプ格納用
		HashMap<Long, AttributeTypeDomain> attrTypeMap = getAttrTypeMapFromList(attTypeList);

		// 貼付け先オブジェクトの下位引継ぎ属性を取得
		List<Long> lowAttrs = AppDocumentUtil.getIntAttrs(nextParentObj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR")); // 下位への引継ぎ属性
		if (lowAttrs != null) {
			for (int i = 0; i < lowAttrs.size(); i++) {
				if (attrTypeMap.containsKey(lowAttrs.get(i))) {

					AttributeTypeDomain attrType = attrTypeMap.get(lowAttrs.get(i));
					switch (attrType.getValueType()) {
					// 文字列型の場合
					case STRING:
						List<String> strAttrs = AppDocumentUtil.getStrAttrs(nextParentObj, attrType.getDefinitionName());
						setLowAttrMap.put(attrType.getId(), strAttrs);
						break;

					// 数値型の場合
					case LONG:
						List<Long> intAttrs = AppDocumentUtil.getIntAttrs(nextParentObj, attrType.getDefinitionName());
						setLowAttrMap.put(attrType.getId(), intAttrs);
						break;

					// 実数値型の場合
					case DOUBLE:
						List<Double> doubleAttrs = AppDocumentUtil.getDoubleAttrs(nextParentObj, attrType.getDefinitionName());
						setLowAttrMap.put(attrType.getId(), doubleAttrs);
						break;

					// 日付型の場合
					case DATE:
						List<Date> dateAttrs = AppDocumentUtil.getDateAttrs(nextParentObj, attrType.getDefinitionName());
						setLowAttrMap.put(attrType.getId(), dateAttrs);
						break;

					// テキスト型の場合
					case TEXT:
						List<String> textAttrs = AppDocumentUtil.getTextAttrs(nextParentObj, attrType.getDefinitionName());
						setLowAttrMap.put(attrType.getId(), textAttrs);
						break;
					}
				}
			}
		}

		long upWfFolderId = 0; // 貼付け先の上位WFフォルダID
		StatusDomain upWfFolderStatus = null; // 上位WFフォルダのステータス

		// 上位WFフォルダID、上位WFフォルダのステータス種別を取得
		// 貼付け先がWF付きフォルダの場合
		if (AppDocumentLogicUtil.isTypeOfFolderWithWorkflow(nextParentObj)) {
			upWfFolderId = nextParentObj.getId();
			upWfFolderStatus = nextParentObj.getStatus();

			// 貼付け先が一般フォルダの場合
		} else {
			upWfFolderId = AppDocumentUtil.getIntAttr(nextParentObj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER")); // 上位WFフォルダ
			if (upWfFolderId != 0) {
				ObjectDomain tmpObj = objectService.getById(upWfFolderId);
				if (tmpObj != null) {
					upWfFolderStatus = tmpObj.getStatus();
				}
			}
		}

		long nextSecurityId = 0; // 下位オブジェクトに適応するセキュリティID

		// ごみ箱内ではない場合、または、ドキュメントの場合
		// ※ タグはごみ箱からの復帰がないため、このif文には含めない
		if (!inRecycleBox || AppDocumentLogicUtil.isTypeOfDocument(obj.getType())) {
			// 貼付け先のセキュリティを引継ぐ
			nextSecurityId = (nextParentObj.getSecurity() != null ? nextParentObj.getSecurity().getId() : 0);
		}

		// 処理高速化用のSecurityDomain格納マップ [key]セキュリティID [value]SecurityDomain
		HashMap<Long, SecurityDomain> securityMap = new HashMap<Long, SecurityDomain>();
		// ごみ箱内の場合
		if (inRecycleBox) {
			// 論理削除解除の場合にはセキュリティの復元があり、都度セキュリティの存在確認すると効率が悪いのでMapに格納する
			List<SecurityDomain> secList = securityService.getList();
			if (secList != null) {
				for (SecurityDomain sec : secList) {
					securityMap.put(sec.getId(), sec);
				}
			}
		}

		// 処理高速化用オブジェクトタイプ格納マップ　[key]オブジェクトタイプID [value]オブジェクトタイプ
		HashMap<Long, ObjectTypeDomain> objTypeMap = new HashMap<Long, ObjectTypeDomain>();
		objTypeMap.put(obj.getType().getId(), obj.getType());

		// 自分および自分以下のオブジェクトに削除すべき上位引継ぎ属性の除去、貼付け先の下位引継ぎ属性を反映 (再帰処理)
		updateObjAttr(obj, (inRecycleBox ? MODE_RETURN : MODE_MOVE), path, setLowAttrMap, delAttrs, upWfFolderId, upWfFolderStatus, nextSecurityId,
					 securityMap);
	}

	/**
	 * 再帰的にObjectDomainの属性を更新/削除する共通処理です。
	 *
	 * <li>本メソッドは再帰的に呼び出されて子ObjectDomainを更新します。
	 * <li>下位に引継ぐ属性値のみを引数parentLowAttrMapに設定して下さい。
	 * <li>削除すべき上位引継ぎ属性のみを引数delTargetAttrsに設定して下さい。
	 * <li>本メソッドはcommit/rollbackは実施しません。(呼び出し側で実施して下さい。)
	 *
	 * @param obj 属性を更新する対象のオブジェクト
	 * @param mode 起動モード(3:論理削除(ごみ箱へ移動))
	 * @param path 設定すべきパス。未設定の場合はnullを設定すること。
	 * @param parentLowAttrMap 「下位への引継ぎ属性」格納用マップ [key]属性タイプID [value]属性の値(配列)
	 * @param delTargetAttrs 削除すべき「上位からの引継ぎ属性」のint配列
	 * @param upWfFolderId 上位WFフォルダIDを更新する場合のみ設定。未設定の場合は0を設定すること。
	 * @param upWfFolderStatus 上位WFフォルダのステータス。未設定の場合はnullを設定すること。
	 * @param secId セキュリティを変更する場合のみ設定。未設定の場合は0を設定すること。
	 * @param securityMap 処理高速化用のSecurityDomainのmap。存在確認用。削除解除モードのみ使用。
	 * @throws Exception
	 */
	private static void updateObjAttr(ObjectDomain obj, int mode, String path, HashMap<Long, Object> parentLowAttrMap, List<Long> delTargetAttrs,
			long upWfFolderId, StatusDomain upWfFolderStatus, long secId, HashMap<Long, SecurityDomain> securityMap) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectDao objectDao = (ObjectDao) context.getBean("objectDao4");
		ObjectService objectService = (ObjectService) context.getBean("objectService4");
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");
		SecurityService securityService = (SecurityService) context.getBean("securityService2");

		// 「論理削除」モード
		if (mode == MODE_DELETE) {
			// 論理削除可能か否かを判定 (不可の場合は例外発生)
			checkDeleteEnable(obj);

			// 削除日時を設定
			Date date = new Date();
			List<Date> dateList = new ArrayList<>();
			dateList.add(date);
			AppDocumentLogicUtil.setAttrDeleteDate(obj, dateList);
		}

		/* パスの設定 */
		if (mode != MODE_UPDATE) {
			if (path != null) {
				if (mode == MODE_COPY) { // コピーの場合は指定パスのみをパス属性に設定する
					AppDocumentUtil.setPath(obj, path); // パス
				} else {
					// コピー以外の場合はパス属性の先頭だけを入れ替える
					ObjectDomain tmpObj = objectService.getById(obj.getId());
					AppDocumentLogicUtil.replaceFirstPath(tmpObj, path); // パス

				}
				path += obj.getName() + "/";
			}
		}

		/* 真の削除すべき上位引継ぎ属性を導出 */

		List<Long> delTargetAttrList = new ArrayList<Long>();
		if (mode == MODE_MOVE || mode == MODE_COPY || mode == MODE_DELETE) {
			// 削除すべき上位引継ぎ属性が「下位に引継ぐ属性値」に含まれる場合は除去する (下位引継ぎ属性を残す)
			if (delTargetAttrs != null) {
				for (int i = 0; i < delTargetAttrs.size(); i++) {
					if (!parentLowAttrMap.containsKey(delTargetAttrs.get(i))) {
						delTargetAttrList.add(delTargetAttrs.get(i));
					}
				}
			}
		}

		// 当該オブジェクトの元々の「上位からの引継ぎ属性」のHashSet [key]属性タイプID
		List<Long> baseHighAttrs = AppDocumentUtil.getIntAttrs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"));

		AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
		attributeTypeCriteria.setObjectTypeId(obj.getType().getId());
		// 対象オブジェクトのオブジェクトタイプが持つ属性タイプを取得
		List<AttributeTypeDomain> attTypeList = attributeTypeService.getList(attributeTypeCriteria);

		// 「属性情報更新」モードの場合は、対象属性がなければ更に探索はしない
		if (mode == MODE_UPDATE && (attTypeList == null || attTypeList.size() == 0)) {
			// 属性を持たない場合、下位引継ぎは発生しないので次ループへ
			return;
		} else {

			boolean targetExistFlag = false; // 引継ぎ属性の存在有無
			// オブジェクトのタイプがフォルダ or ドキュメントのときに確認する
			if (AppDocumentLogicUtil.isTypeOfDocument(obj.getType()) || AppDocumentLogicUtil.isTypeOfFolder(obj.getType())) {
				if (attTypeList != null) {
					for (AttributeTypeDomain attrType : attTypeList) {
						if (parentLowAttrMap.containsKey(attrType.getId())) {
							targetExistFlag = true;
							break;
						}
					}
				}
			}
			// 親の下位引継ぎ属性に該当する属性を持たない場合、元々の上位引継ぎ属性を削除
			if (!targetExistFlag) {
				if (baseHighAttrs != null && baseHighAttrs.size() > 0) {
					// 元々の上位引継ぎ属性を削除
					AppDocumentUtil.deleteAttribute(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_FROM_HIGH_ATTR")); // 上位からの引継ぎ属性
				}
				// 「属性情報更新」モードの場合、次ループへ
				if (mode == MODE_UPDATE) {
					return;
				}
			}
		}

		// 以下の処置はドキュメント・フォルダについてのみ実施する
		HashMap<Long, Object> lowAttrMap = new HashMap<Long, Object>(); // 対象オブジェクトの下位引継ぎ属性
		// 格納用マップ
		// [key]属性タイプID [value]属性の値(配列)
		List<Long> nextDelTargetAttrList = new ArrayList<Long>(); // 次の下位階層での「削除すべき上位引継ぎ属性」のリスト
		List<Long> truthParentLowAttrList = new ArrayList<Long>(); // 真の「上位からの引継ぎ属性」格納用リスト

		if (AppDocumentLogicUtil.isTypeOfDocument(obj.getType()) || AppDocumentLogicUtil.isTypeOfFolder(obj.getType())) {
			// 対象オブジェクトの名称割当て属性と親オブジェクトの下位引継ぎ属性が重複した場合はエラー
			long nameAllocateId = AppDocumentUtil.getIntAttr(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_NAME_ATTR")); // 名称割当て属性
			if (nameAllocateId != 0 && parentLowAttrMap.containsKey(nameAllocateId)) {
				Object[] message = { (attributeTypeService.getById(nameAllocateId)).getName(), obj.getName() };
				// 下位引継ぎ属性【{0}】と、【{1}】の「名称割当て属性」が重複しています。
				throw new EIMException("EIM.ERROR.LOGIC.ATTRINFO.REPETITION.LOWATTR.AND.NAME.ALLOCATION", message);
			}

			/* 対象オブジェクトの属性値取得 */

			HashMap<Long, Object> attrMap = new HashMap<Long, Object>(); // [key]属性タイプID
			// [value]属性の値(配列)
			List<AttributeDomain> attrList = obj.getAttributeList();
			if (attrList != null) {
				for (AttributeDomain attr : attrList) {

					// 対象オブジェクトの属性値に入力値を設定
					switch (attr.getAttributeType().getValueType()) {
					// 文字列型の場合
					case STRING:
						attrMap.put(attr.getAttributeType().getId(), attr.getStringList());
						break;
					// 数値型の場合
					case LONG:
						attrMap.put(attr.getAttributeType().getId(), attr.getLongList());
						break;
					// double数値型の場合
					case DOUBLE:
						attrMap.put(attr.getAttributeType().getId(), attr.getDoubleList());
						break;
					// 日付型の場合
					case DATE:
						attrMap.put(attr.getAttributeType().getId(), attr.getDateList());
						break;
					// テキスト型の場合
					case TEXT:
						attrMap.put(attr.getAttributeType().getId(), attr.getTextList());
						break;
					}
				}
			}

			// 対象オブジェクトの元々の下位引継ぎ属性を取得
			List<Long> baseLowAttrs = AppDocumentUtil.getIntAttrs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR")); // 下位への引継ぎ属性
			if (baseLowAttrs != null) {
				for (int i = 0; i < baseLowAttrs.size(); i++) {

					// 真の削除すべき「上位からの引継ぎ属性」の場合は格納しない
					if (delTargetAttrList.contains(baseLowAttrs.get(i))) {
						continue;
					}

					// 親から引継ぐことになる属性値(＝上位引継ぎ属性)の場合はここではまだ格納しない
					if (!parentLowAttrMap.containsKey(baseLowAttrs.get(i))) {
						// 対象オブジェクトの下位引継ぎ属性に格納
						lowAttrMap.put(baseLowAttrs.get(i), attrMap.get(baseLowAttrs.get(i)));
					}

					// 親から引継ぐことになる属性値と一致する、かつ、元々の上位引継ぎ属性に含まれない
					// (つまり自分が起点となる下位引継ぎ属性と上位からの引継ぎ属性がぶつかった)場合はエラー
					else if (baseHighAttrs == null || !baseHighAttrs.contains(baseLowAttrs.get(i))) {
						AttributeTypeDomain attrType = attributeTypeService.getById(baseLowAttrs.get(i));
						Object[] message = { attrType.getName(), obj.getName() };
						// 下位引継ぎ属性【{0}】が、【{1}】の下位引継ぎ属性と重複しています。
						throw new EIMException("EIM.ERROR.LOGIC.REPETITION.SUCCESSION.ATTR", message);
					}
				}
			}

			for (AttributeTypeDomain attrType : attTypeList) {

				// 対象オブジェクトのオブジェクトタイプが引継ぎ属性を保持する場合
				if (parentLowAttrMap.containsKey(attrType.getId())) {

					Object tmpObjs = parentLowAttrMap.get(attrType.getId());
					if (tmpObjs == null) {
						// 親の属性値が空の場合は削除
						objectService.removeAttribute(obj, attrType);
					}

					switch (attrType.getValueType()) {

					// 文字列型の場合
					case STRING:
						// テキスト型の場合
					case TEXT:
						AppDocumentUtil.setAttributeStrings(obj, attrType, (List<String>) tmpObjs);
						break;
					// 数値型の場合
					case LONG:
						AppDocumentUtil.setAttributeLongs(obj, attrType, (List<Long>) tmpObjs);
						break;
					// 実数型の場合
					case DOUBLE:
						AppDocumentUtil.setAttributeDoubles(obj, attrType, (List<Double>) tmpObjs);
						break;
					// 日付型の場合
					case DATE:
						AppDocumentUtil.setAttributeDates(obj, attrType, (List<Date>) tmpObjs);
						break;
					}

					// 対象オブジェクトの「下位への引継ぎ属性」に格納
					lowAttrMap.put(attrType.getId(), parentLowAttrMap.get(attrType.getId()));

					// 対象オブジェクトの「上位からの引継ぎ属性」に格納
					truthParentLowAttrList.add(attrType.getId());

					// 「属性情報更新」モード以外の場合
				} else if (mode != MODE_UPDATE) {
					if (delTargetAttrList.contains(attrType.getId())) {

						// 削除すべき上位引継ぎ属性を削除
						objectService.removeAttribute(obj, attrType);
						// 該当する属性タイプを持つ場合、次の下位階層での「削除すべき上位引継ぎ属性」に追加
						nextDelTargetAttrList.add(attrType.getId());
					}
				}
			}
		}

		// 「属性情報更新」モード以外の場合、属性「上位WFフォルダ」、ステータスを更新する
		if (mode != MODE_UPDATE) {
			// 上位WFフォルダが存在する場合
			if (upWfFolderId != 0) {

				// 自分自身がWF付きフォルダ、WF付きドキュメントの場合はエラー
				if (AppDocumentLogicUtil.isTypeOfFolderWithWorkflow(obj) || AppDocumentLogicUtil.isTypeOfDocumentWithWorkflow(obj)) {
					// ワークフロー付きフォルダの下に、ワークフロー付きのフォルダまたはドキュメントは配置できません。
					throw new EIMException("EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCFOL");

				} else {
					// 属性「上位WFフォルダ」の設定
					AppDocumentUtil.setAttrLong(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"), upWfFolderId); // 上位WFフォルダ

					// ステータスの設定
					if (upWfFolderStatus != null) {
						objectDao.setStatus(obj, upWfFolderStatus);
					}
				}
				// 上位WFが存在しない場合
			} else {
				if (AppDocumentLogicUtil.isTypeOfFolderWithWorkflow(obj)) {

					// ここから上位WFフォルダのIDを下位に引継ぐ
					upWfFolderId = obj.getId();

					// ここからステータスを下位に引継ぐ
					upWfFolderStatus = obj.getStatus();

					// WFを持たないフォルダ、またはドキュメントの場合
				} else if (!AppDocumentLogicUtil.isTypeOfDocumentWithWorkflow(obj)) {
					// 上位WFフォルダ
					List<Long> tmpInts = AppDocumentUtil.getIntAttrs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"));
					if (tmpInts != null) {
						// 属性「上位WFフォルダ」の削除
						AppDocumentUtil.deleteAttribute(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"));
					}
					if (obj.getStatus() != null) {
						// ステータスの解除
						setStatusNull(obj);
					}
				}
			}
		}

		// 親オブジェクトの下位引継ぎ属性を対象オブジェクトの上位引継ぎ属性に設定
		// ※ 対象オブジェクトがタグの場合は実施しない(属性値を下位に引き継がないため)
		if (AppDocumentLogicUtil.isTypeOfDocument(obj.getType()) || AppDocumentLogicUtil.isTypeOfFolder(obj.getType()))
			AppDocumentUtil.setAttrLongs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"), truthParentLowAttrList); // 上位からの引継ぎ属性

		// フォルダ、またはワークスペースの場合
		List<ObjectDomain> childRelList = null;
		if (AppDocumentLogicUtil.isTypeOfFolder(obj.getType()) || AppDocumentLogicUtil.isTypeOfWorkspace(obj.getType())) {

			// 対象オブジェクトの属性「下位への引継ぎ属性」を設定
			AppDocumentUtil.setAttrLongs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR"), new ArrayList<Long>(Arrays.asList(lowAttrMap
					.keySet().toArray(new Long[0])))); // 下位への引継ぎ属性

			// 対象オブジェクトのリレーションリストを取得
			// リレーションタイプ
			RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();
			childRelList = AppDocumentUtil.getChildObjectByRelTypeAndParent(relationTypeDomain, obj, null);
		}

		/* セキュリティの設定 */

		// セキュリティの変更により対象Objectを操作出来なくなる場合があるため、セキュリティは最後尾で設定する
		switch (mode) {
		// 「論理削除」モード
		case MODE_DELETE:

			// フォルダの場合のみ、前セキュリティを保存する
			if (obj.getSecurity() != null && AppDocumentLogicUtil.isTypeOfFolder(obj.getType())) {
				// ワークスペース固有ごみ箱配下からの論理削除の場合は前セキュリティを変更しない
				// (「/ごみ箱/」から始まる場合はシステムのごみ箱への論理削除時)
				if (!path.startsWith("/" + ConfigUtils.getByKey("OBJECT_TYPE_NAME_RECYCLE") + "/")) {
					AppDocumentUtil.setAttrLong(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_PREV_SEC"), obj.getSecurity().getId()); // 前セキュリティ
				}

			}
			// 指定のセキュリティを設定
			if (secId != 0) {
				objectService.setSecurity(obj, new SecurityDomain(secId));
			}
			break;
			// 「移動」「コピー」モード
		case MODE_MOVE:
		case MODE_COPY:
			// 指定のセキュリティを設定
			if (secId != 0) {
				objectService.setSecurity(obj, new SecurityDomain(secId));
			}
			break;
			// 「論理削除解除」モード
		case MODE_RETURN:

			// フォルダの場合
			if (AppDocumentLogicUtil.isTypeOfFolder(obj.getType())) {

				// 前セキュリティの取得
				long beforeSecId = AppDocumentUtil.getIntAttr(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_PREV_SEC"));

				// 前セキュリティを保持する場合
				if (beforeSecId != 0 && securityMap.containsKey(beforeSecId)) {

					// 前セキュリティを当該オブジェクトのセキュリティに設定
					objectService.setSecurity(obj, securityMap.get(beforeSecId));

					// 属性「前セキュリティ」の削除
					objectService.removeAttribute(obj, attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_FOLDER_PREV_SEC")));

					// 下位ドキュメントには前セキュリティが適応される
					secId = beforeSecId;

				// 前セキュリティを保持しない、または、前セキュリティがもうDBには存在しない場合
				} else {

					// systemセキュリティを設定
					SecurityDomain systemSecurity = securityService.getByDefinitionName(ConfigUtils.getByKey("SECURITY_NAME_SYSTEM"));
					objectService.setSecurity(obj, systemSecurity);

					// 既にDBに存在しない前セキュリティは削除
					if (!securityMap.containsKey(beforeSecId)) {
						objectService.removeAttribute(obj, attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_FOLDER_PREV_SEC")));
					}

					// 下位ドキュメントにはsystemセキュリティが適応される
					secId = systemSecurity.getId();
				}
			// ドキュメントの場合
			} else {
				// 指定のセキュリティを設定
				if (secId != 0) {
					objectService.setSecurity(obj, new SecurityDomain(secId));
				}
			}
			break;
		}

		// 子オブジェクトへの下位引継ぎ処理
		if (childRelList != null) {
			for (ObjectDomain childObj : childRelList) {
				if (mode == MODE_DELETE && AppDocumentLogicUtil.isTypeOfTag(childObj.getType())) {
					// タグの論理削除時は属性更新不要のためcontinue
					continue;
				}
				// 本メソッドの再帰呼び出し
				updateObjAttr(childObj, mode, path, lowAttrMap, nextDelTargetAttrList, upWfFolderId, upWfFolderStatus, secId, securityMap);
			}
		}
	}

	/**
	 * 論理削除可能か否かを判定します。
	 *
	 * <li>システム日付 ＜＝ 属性値「有効期限」の場合、例外を発生させます。
	 * <li>ロックユーザが存在する場合、例外を発生させます。
	 * <li>ステータス条件が不一致の場合、例外を発生させます。
	 *
	 * @param obj 有効期限を判定する対象のObjectDomain
	 * @throws Exception
	 */
	public static void checkDeleteEnable(ObjectDomain obj) throws Exception {

		// 有効期限のチェック
		AttributeDomain attributeDomain = obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_FOLDER_EFFECT_DATE"));
		if (attributeDomain != null) {
			Date effectDate = attributeDomain.getDate();
			if (effectDate != null) {
				// システム日付 ＜＝ 対象オブジェクトの属性値「有効期限」の場合はエラー
				if (!AppDocumentLogicUtil.judgeExpirationDate(effectDate)) {
					// 有効なドキュメント、またはフォルダは削除できません。
					throw new EIMException("EIM.ERROR.LOGIC.NOTDEL.EFFECTIVE.DOCFOL");
				}
			}
		}

		// WFフォルダの場合
		if (AppDocumentLogicUtil.isTypeOfFolderWithWorkflow(obj)) {
			long stsKind = obj.getStatus().getType().getBase().getId();

			// ステータスが「編集中」でも「公開済」でもない場合
			if (stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING && stsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
				// 削除権限がありません。
				throw new EIMException("EIM.ERROR.LOGIC.NODELETEROLE");
			}
		}

		// ドキュメントの場合
		if (AppDocumentLogicUtil.isTypeOfDocument(obj.getType())) {

			// ロックユーザが存在する場合
			if (obj.getLockUser() != null) {
				// {0}　は、改訂中です。削除できません。
				throw new EIMException("EIM.ERROR.LOGIC.NODELETE.REVISING", new Object[] { obj.getName() });
			}

			// WF付きドキュメントの場合
			if (AppDocumentLogicUtil.isTypeOfDocumentWithWorkflow(obj)) {
				long stsKind = obj.getStatus().getType().getBase().getId();

				// ステータスが「編集中」でも「公開済」でもない場合
				if (stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING && stsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
					// {0}　は、承認中です。削除できません。
					throw new EIMException("EIM.ERROR.LOGIC.NODELETE.APPROVAL", new Object[] { obj.getName() });

					// 編集中かつ履歴番号が0より大きい場合
				} else if (stsKind == AppConstant.STATUS_TYPE_KIND_ID_EDITTING && obj.getRevision() > 0) {
					// {0}　は、改訂中です。削除できません。
					throw new EIMException("EIM.ERROR.LOGIC.NODELETE.REVISING", new Object[] { obj.getName() });
				}
				// WFなしドキュメント
			} else {
				Map<Long, ObjectDomain> revObjectMap = VersionUtils.getVersion(obj);
				List<ObjectDomain> objectDomains = Arrays.asList(revObjectMap.values().toArray(new ObjectDomain[0]));
				if (objectDomains.size() > 1) {
					ObjectDomain former = objectDomains.get(objectDomains.size() - 2);
					if (former != null && former.isLatest()) {
						// {0}は、改訂中です。削除できません。
						throw new EIMException("EIM.ERROR.LOGIC.NODELETE.REVISING", new Object[] { obj.getName() });
					}
				}
			}
		}
	}

	/**
	 * ObjectDomainを更新します。
	 *
	 * <li>対象の下位階層にObjectDomainが存在する場合、下位ObjectDomainに対して引継ぎ属性の設定を行います。 <li>
	 * 本メソッドはcommit/rollbackは実施しません。(呼び出し側で実施して下さい。)
	 *
	 * @param obj 更新対象のObjectDomain
	 * @param relType リレーションタイプ
	 * @throws Exception
	 */
	public static void updateAttribute(ObjectDomain obj, RelationTypeDomain relType) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

		// 親リレーションの取得
		List<RelationDomain> parentRelList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relType, obj, new AccessRoleTypeDomain("READ"));

		// 親オブジェクトの下位引継ぎ属性を格納したHashSet [key]属性タイプID
		HashSet<Long> parentLowAttrSet = new HashSet<Long>();
		ObjectDomain parentObj = null;
		if (parentRelList != null && parentRelList.size() > 0) {

			// 親オブジェクトの取得
			parentObj = parentRelList.get(0).getParent();

			// 親オブジェクトの下位引継ぎ属性を取得
			List<Long> parentLowAttrs = AppDocumentUtil.getIntAttrs(parentObj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR"));
			if (parentLowAttrs != null) {
				parentLowAttrSet.addAll(parentLowAttrs);
			}
		}

		AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
		attributeTypeCriteria.setObjectTypeId(obj.getType().getId());
		// 対象オブジェクトのオブジェクトタイプに設定されている属性タイプ一覧を取得
		List<AttributeTypeDomain> attTypeList = attributeTypeService.getList(attributeTypeCriteria);

		// 対象オブジェクトの「上位引継ぎ属性」格納用
		List<Long> myselfUpAttrList = new ArrayList<Long>();
		// 対象オブジェクトの「下位引継ぎ属性」格納用マップ [key]属性タイプID [value]属性の値(配列) 値がない場合はnull
		HashMap<Long, Object> myselfLowAttrMap = new HashMap<Long, Object>();

		// 元々の下位引継ぎ属性を取得
		List<Long> baseLowAttrs = AppDocumentUtil.getIntAttrs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR"));

		// 対象がドキュメント、またはフォルダかを判定する
		boolean isTypeOfDocumentOrFolder = false;
		if(AppDocumentLogicUtil.isTypeOfDocument(obj.getType()) || AppDocumentLogicUtil.isTypeOfFolder(obj.getType())){
			isTypeOfDocumentOrFolder = true;
		}

		for (AttributeTypeDomain attrType : attTypeList) {

			if (ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE").equals(attrType.getDefinitionName())) {
				continue;
			}

			// 以下はドキュメント・フォルダのときのみ実施
			// 属性タイプIDが上位ObjectDomainの「下位への引継ぎ属性」と一致
			if ( isTypeOfDocumentOrFolder && parentLowAttrSet.contains(attrType.getId()) ) {

				// 上位ObjectDomainの属性値を対象ObjectDomainに設定
				switch (attrType.getValueType()) {

				// 文字列型の場合
				case STRING:

					List<String> strAttrs = AppDocumentUtil.getStrAttrs(parentObj, attrType.getDefinitionName());
					AppDocumentUtil.setAttributeStrings(obj, attrType, strAttrs);
					myselfLowAttrMap.put(attrType.getId(), strAttrs);
					break;

				// 数値型の場合
				case LONG:

					List<Long> intAttrs = AppDocumentUtil.getIntAttrs(parentObj, attrType.getDefinitionName());
					AppDocumentUtil.setAttributeLongs(obj, attrType, intAttrs);
					myselfLowAttrMap.put(attrType.getId(), intAttrs);
					break;

				// double数値型の場合
				case DOUBLE:

					List<Double> doubleAttrs = AppDocumentUtil.getDoubleAttrs(parentObj, attrType.getDefinitionName());
					AppDocumentUtil.setAttributeDoubles(obj, attrType, doubleAttrs);
					myselfLowAttrMap.put(attrType.getId(), doubleAttrs);
					break;

				// 日付型の場合
				case DATE:

					List<Date> dateAttrs = AppDocumentUtil.getDateAttrs(parentObj, attrType.getDefinitionName());
					AppDocumentUtil.setAttributeDates(obj, attrType, dateAttrs);
					myselfLowAttrMap.put(attrType.getId(), dateAttrs);
					break;

				// テキスト型の場合
				case TEXT:

					List<String> textAttrs = AppDocumentUtil.getTextAttrs(parentObj, attrType.getDefinitionName());
					AppDocumentUtil.setAttributeStrings(obj, attrType, textAttrs);
					myselfLowAttrMap.put(attrType.getId(), textAttrs);
					break;
				}
				// 対象オブジェクトの上位引継ぎ属性、下位引継ぎ属性に当該属性タイプIDを設定
				myselfUpAttrList.add(attrType.getId());
				continue;
			}

			// 属性情報を取得
			AttributeDomain item = obj.getAttribute(attrType.getDefinitionName());

			// 対象オブジェクトの属性に該当する入力値が送られなかった場合
			if (item != null) {

				// 渡されてきたAttributeTypeDomainは不完全な場合があるので
				// 取得したAttributeTypeDomainに入れ替える
				item.setAttributeType(attrType);

				AppDocumentUtil.setAttr(obj, item);

				// その属性は元々の「下位引継ぎ属性」である場合
				if (baseLowAttrs != null && baseLowAttrs.contains(attrType.getId())) {

					// 元々の属性値をそのまま引継ぐべき「下位引継ぎ属性」に設定
					switch (attrType.getValueType()) {

					// 文字列型の場合
					case STRING:

						List<String> strAttrs = AppDocumentUtil.getStrAttrs(obj, attrType.getDefinitionName());
						myselfLowAttrMap.put(attrType.getId(), strAttrs);
						break;

					// 数値型の場合
					case LONG:

						List<Long> intAttrs = AppDocumentUtil.getIntAttrs(obj, attrType.getDefinitionName());
						myselfLowAttrMap.put(attrType.getId(), intAttrs);
						break;

					// double数値型の場合
					case DOUBLE:

						List<Double> doubleAttrs = AppDocumentUtil.getDoubleAttrs(obj, attrType.getDefinitionName());
						myselfLowAttrMap.put(attrType.getId(), doubleAttrs);
						break;

					// 日付型の場合
					case DATE:

						List<Date> dateAttrs = AppDocumentUtil.getDateAttrs(obj, attrType.getDefinitionName());
						myselfLowAttrMap.put(attrType.getId(), dateAttrs);
						break;

					// テキスト型の場合
					case TEXT:

						List<String> textAttrs = AppDocumentUtil.getTextAttrs(obj, attrType.getDefinitionName());
						myselfLowAttrMap.put(attrType.getId(), textAttrs);
						break;
					}
				}
				// 属性の設定値がない場合
			} else {

				// 当該属性を対象オブジェクトから削除
				objectService.removeAttribute(obj, attrType);

				// 下位引継ぎ属性に指定されている場合
				if (baseLowAttrs != null && baseLowAttrs.contains(attrType.getId())) {
					myselfLowAttrMap.put(attrType.getId(), null);
				}
			}
		}

		// 上位引継ぎ属性が存在する場合
		if (myselfUpAttrList.size() > 0) {
			// 上位引継ぎ属性を設定
			AppDocumentUtil.setAttrLongs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"), myselfUpAttrList);

		} else {
			// 元々の上位引継ぎ属性を削除
			List<Long> baseHighAttrs = AppDocumentUtil.getIntAttrs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"));
			if (baseHighAttrs != null) {
				AppDocumentUtil.deleteAttribute(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"));
			}
		}

		// フォルダ、またはワークスペースの場合のみ下位引継ぎ処理を実施
		if (AppDocumentLogicUtil.isTypeOfFolder(obj.getType()) || AppDocumentLogicUtil.isTypeOfWorkspace(obj.getType())) {

			// 下位引継ぎ属性が存在する場合
			if (myselfLowAttrMap.size() > 0) {
				// 下位引継ぎ属性を設定
				AppDocumentUtil.setAttrLongs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR"), Arrays.asList(myselfLowAttrMap.keySet()
						.toArray(new Long[0])));
			} else {
				// 元々の下位引継ぎ属性を削除
				if (baseLowAttrs != null) {
					AppDocumentUtil.deleteAttribute(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR"));
				}
			}

			// 子オブジェクトのリレーションリストを取得
			List<ObjectDomain> childRelList = AppDocumentUtil.getChildObjectByRelTypeAndParent(relType, obj, null);

			if (childRelList != null) {
				for (ObjectDomain childObj : childRelList) {
					// 子オブジェクトの属性を更新/削除します。 (再帰処理)
					updateObjAttr(childObj, MODE_UPDATE, null, myselfLowAttrMap, null, 0, null, 0, null);
				}
			}
		}
	}

	/**
	 * 属性タイプのリストから属性タイプのHashMapを生成します。
	 *
	 * @param attTypeList 属性タイプのリスト
	 * @return 属性タイプID格納Set [key]属性タイプID [value]属性タイプ
	 * @throws Exception
	 */
	private static HashMap<Long, AttributeTypeDomain> getAttrTypeMapFromList(List<AttributeTypeDomain> attTypeList) throws Exception {

		HashMap<Long, AttributeTypeDomain> map = new HashMap<Long, AttributeTypeDomain>();
		if (attTypeList != null) {
			for (AttributeTypeDomain attrType : attTypeList) {
				map.put(attrType.getId(), attrType);
			}
		}
		return map;
	}

	/**
	 * 「コピー＆ペースト」で生成されたObjectDomainの属性情報を更新します。
	 *
	 * <li>新規生成したオブジェクトは、属性を継承してから設定して下さい。
	 * <li>元々の上位引継ぎ属性を削除します。
	 * <li>貼付け先の下位引継ぎ属性を反映します。
	 * <li>本メソッドは対象がドキュメントの場合のみを前提とします。
	 *
	 * @param obj コピーして新規生成されたObjectDomain
	 * @param nextParentObj 貼付け先のObjectDomain
	 * @param path 設定すべきパス
	 * @throws Exception
	 */
	public static void updateAttributeForCopy(ObjectDomain obj, ObjectDomain nextParentObj, String path) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

		// 元々の上位引継ぎ属性の取得 → 削除すべき上位引継ぎ属性
		List<Long> delAttrs = AppDocumentUtil.getIntAttrs(obj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"));	// 上位からの引継ぎ属性

		// 下位に引継ぐ属性値のMap [key]属性タイプID [value]属性の値(配列)
		HashMap<Long, Object> setLowAttrMap = new HashMap<Long, Object>();

		// 貼付け先オブジェクトのオブジェクトタイプに設定されている属性タイプ一覧を取得
		AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
		attributeTypeCriteria.setObjectTypeId(nextParentObj.getType().getId());
		List<AttributeTypeDomain> attTypeList = attributeTypeService.getList(attributeTypeCriteria);

		// 貼付け先オブジェクトの属性タイプ格納用 [key]属性タイプID [value]属性
		HashMap<Long, AttributeTypeDomain> attrTypeMap = getAttrTypeMapFromList(attTypeList);

		// 貼付け先オブジェクトの下位引継ぎ属性を取得
		List<Long> lowAttrs = AppDocumentUtil.getIntAttrs(nextParentObj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR"));	// 下位への引継ぎ属性
		if (lowAttrs != null) {
			for (int i = 0 ; i < lowAttrs.size() ; i++) {
				if (attrTypeMap.containsKey(lowAttrs.get(i))) {

					AttributeTypeDomain attrType = attrTypeMap.get(lowAttrs.get(i));
					switch (attrType.getValueType()) {

						// 文字列型の場合
						case STRING:
							List<String> strAttrs = AppDocumentUtil.getStrAttrs(nextParentObj, attrType.getDefinitionName());
							setLowAttrMap.put(attrType.getId(), strAttrs);
							break;

						// 数値型の場合
						case LONG:
							List<Long> intAttrs = AppDocumentUtil.getIntAttrs(nextParentObj, attrType.getDefinitionName());
							setLowAttrMap.put(attrType.getId(), intAttrs);
							break;

						// 実数値型の場合
						case DOUBLE:
							List<Double> doubleAttrs = AppDocumentUtil.getDoubleAttrs(nextParentObj, attrType.getDefinitionName());
							setLowAttrMap.put(attrType.getId(), doubleAttrs);
							break;

						// 日付型の場合
						case DATE:
							List<Date> dateAttrs = AppDocumentUtil.getDateAttrs(nextParentObj, attrType.getDefinitionName());
							setLowAttrMap.put(attrType.getId(), dateAttrs);
							break;

						// テキスト型の場合
						case TEXT:
							List<String> textAttrs = AppDocumentUtil.getTextAttrs(nextParentObj, attrType.getDefinitionName());
							setLowAttrMap.put(attrType.getId(), textAttrs);
							break;
					}
				}
			}
		}

		long upWfFolderId = 0;		// 貼付け先の上位WFフォルダID
		StatusDomain upWfFolderStatus = null;			// 上位WFフォルダのステータス

		// 上位WFフォルダID、上位WFフォルダのステータスを取得
		// 貼付け先がWF付きフォルダの場合
		if (AppDocumentLogicUtil.isTypeOfFolderWithWorkflow(nextParentObj)) {
			upWfFolderId = nextParentObj.getId();
			upWfFolderStatus = nextParentObj.getStatus();

			// 貼付け先が一般フォルダの場合
		} else {
			upWfFolderId = AppDocumentUtil.getIntAttr(nextParentObj, ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER")); // 上位WFフォルダ
			if (upWfFolderId != 0) {
				ObjectDomain tmpObj = objectService.getById(upWfFolderId);
				if (tmpObj != null) {
					upWfFolderStatus = tmpObj.getStatus();
				}
			}
		}

		// 貼付け先のセキュリティを引継ぐ
		long nextSecurityId = (nextParentObj.getSecurity() != null ? nextParentObj.getSecurity().getId() : 0);

		// 自分および自分以下のオブジェクトに削除すべき上位引継ぎ属性の除去、貼付け先の下位引継ぎ属性を反映 (再帰処理)
		updateObjAttr(obj, MODE_COPY, path, setLowAttrMap, delAttrs, upWfFolderId, upWfFolderStatus, nextSecurityId, null);

		// 元のオブジェクトに設定された継承すべきでない属性を削除
		deleteNonInheritAttribute(obj);

	}

	/**
	 * 非継承の属性を削除します。
	 *
	 * <li>本メソッドは対象がドキュメントの場合のみを前提とします。
	 *
	 * @param obj 属性を削除するObjectDomain
	 * @throws Exception
	 */
	private static void deleteNonInheritAttribute(ObjectDomain obj) throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

		// 非継承の属性を全て削除する
		for (int i = 0; i < AppConstant.NONINHERIT_ATTRIBUTE_DEFNAME.length; i++) {
			AttributeTypeDomain attributeType = attributeTypeService.getByDefinitionName(AppConstant.NONINHERIT_ATTRIBUTE_DEFNAME[i]);
			objectService.removeAttribute(obj, attributeType);
		}
	}

	/**
	 * オブジェクトのステータスを解除します。
	 *
	 * @param object オブジェクト
	 * @throws Exception
	 */
	private static void setStatusNull(ObjectDomain object) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectDao objectDao = (ObjectDao) context.getBean("objectDaoForUtil");
		objectDao.setStatus(object, new StatusDomain());

		return;
	}

	/**
	 * 削除可能か否かを判定します。
	 *
	 * @param obj 削除する対象のObjectDomain
	 * @param parentObj 対象の親ObjectDomain
	 * @throws Exception
	 */
	public static void checkStatus(ObjectDomain obj, ObjectDomain parentObj) throws Exception {

		// 親オブジェクトのステータス種別ID
		long parentStsKind = AppConstant.STATUS_TYPE_KIND_ID_NONE;
		if (parentObj != null && parentObj.getStatus() != null) {
			parentStsKind = parentObj.getStatus().getType().getBase().getId();
		}

		// 上位WFフォルダが存在する場合
		if (parentObj != null
				&& (AppDocumentLogicUtil.isTypeOfFolderWithWorkflow(parentObj) || AppDocumentLogicUtil.isTypeOfFolderUnderFolderWithWorkflow(parentObj))) {
			// 上位WFのステータスが「編集中」でも「公開済み」でもない場合はエラー
			if ((parentStsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) && parentStsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC){
				// 削除権限がありません。
				throw new EIMException("EIM.ERROR.LOGIC.NODELETEROLE");
			}
			// 上位WFフォルダが存在しない、かつ、対象がWFを持つ場合
		} else if (AppDocumentLogicUtil.isTypeOfFolderWithWorkflow(obj) || AppDocumentLogicUtil.isTypeOfDocumentWithWorkflow(obj)) {

			// 対象オブジェクトのステータス種別ID
			long stsKind = obj.getStatus() != null ? obj.getStatus().getType().getBase().getId() : AppConstant.STATUS_TYPE_KIND_ID_NONE;

			// 対象のステータスが「編集中」でも「公開済み」でもない場合はエラー
			if (stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING && stsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
				// 削除できるステータスではありません。
				throw new EIMException("EIM.ERROR.LOGIC.CANT.DELETE.STATUS");
			}
		}
	}

}
