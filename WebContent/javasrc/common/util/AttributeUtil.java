package common.util;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import common.bo.AttributeUpdater;
import common.bo.AttributeUpdaterItem;
import common.bo.AttributeValueMaster;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMSecurity;
import eim.bo.EIMStatus;
import eim.bo.EIMValueType;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMXmlConfigAdminAuth;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;

/**
 *
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
	 * 属性情報画面で入力された属性情報でEIMObjectを更新します。
	 *
	 * <li>属性情報画面でユーザの入力不可の属性(デフォルト属性等)は、引数「AttributeUpdaterItemのリスト」には設定しないで下さい。
	 * <li>名称割当て属性に指定した属性の値は、事前に引数「オブジェクト名」に設定しておいて下さい。
	 * <li>対象の下位階層にEIMObjectが存在する場合、下位EIMObjectに対して引継ぎ属性の設定を行います。
	 * <li>オブジェクト名が変更された場合のみ、アクセス履歴「改名」を追加します。
	 * <li>本メソッドはcommit/rollbackは実施しません。(呼び出し側で実施して下さい。)
	 *
	 * @param sess EIMSessionインスタンス
	 * @param updater 属性更新情報の管理クラス
	 * @throws Exception
	 */
	public static void updateAttribute(EIMSession sess, AttributeUpdater updater) throws Exception {

		if (updater == null) {
			// 入力情報を取得できませんでした。
			throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTEXIST.INPUTDATA");
		}

		EIMObject obj = ObjectUtils.getObjectById(sess, updater.getObjId());

		if(obj.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"))
				&& EIMXmlConfigAdminAuth.hasSpecifiedAuth(sess.getUser(), AppConstant.ADMIN_AUTH_ID_WORKSPACE)){
			// ワークスペースの属性情報更新 かつ「ワークスペース作成権限」をユーザが持つ場合は
			// Objectの更新権限チェック不要

		}else{
			if (obj == null || !SecurityUtils.authorized(sess, obj,sess.getUser(), EIMAccessRole.UPDATE)) {
				// 指定のドキュメント、フォルダ、またはタグを取得できませんでした。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOLTAG");
			}
		}

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// オブジェクト名が変更された場合
		if (updater.getObjName() != null && !obj.getName().equals(updater.getObjName())) {
			// 改名処理
			if (helper.isTypeOfWorkspace(obj.getType())) {
				// ワークスペース
				AppLogicUtil.renameObjectForWorkSpace(sess, obj, updater.getObjName());
				// SearchFramework 検索FW更新通知 対象：オブジェクト + 配下オブジェクト
				AppUpdateNoticeUtils.updateNoticeInsert(obj.getId(), "SEARCHFW_UPDATE_ATTR_WORKSPACE");
				AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, obj,
							"SEARCHFW_UPDATE_ATTR_CHILD_FOLDER", "SEARCHFW_UPDATE_ATTR_CHILD_DOCUMENT",
							"SEARCHFW_UPDATE_ATTR_CHILD_TAG", "SEARCHFW_UPDATE_ATTR_CHILD_DOCUMENTLINK");
			} else {
				// フォルダ、ドキュメント、タグ
				AppLogicUtil.renameObject(sess, obj, updater.getObjName(), helper);
				// SearchFramework 検索FW更新通知 対象：フォルダ・ドキュメント・タグ
				AppUpdateNoticeUtils.updateNoticeInsertObject(sess, obj,
						"SEARCHFW_UPDATE_ATTR_RENAME_FOLDER", "SEARCHFW_UPDATE_ATTR_DOCUMENT",
						"SEARCHFW_UPDATE_ATTR_TAG", null);

				if(helper.isTypeOfFolder(obj.getType())) {
					// SearchFramework 検索FW更新通知 対象：配下のフォルダ・ドキュメント・タグ・ドキュメントリンク
					AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, obj,
							"SEARCHFW_UPDATE_ATTR_CHILD_FOLDER", "SEARCHFW_UPDATE_ATTR_CHILD_DOCUMENT",
							"SEARCHFW_UPDATE_ATTR_CHILD_TAG", "SEARCHFW_UPDATE_ATTR_CHILD_DOCUMENTLINK");
				}
			}
		}
		else
		{
			// 改名しない場合
			// SearchFramework 検索FW更新通知 対象：フォルダ、ドキュメント、タグ、ワークスペース
			AppUpdateNoticeUtils.updateNoticeInsertObject(sess, obj,
					"SEARCHFW_UPDATE_ATTR_FOLDER", "SEARCHFW_UPDATE_ATTR_DOCUMENT",
					"SEARCHFW_UPDATE_ATTR_TAG", "SEARCHFW_UPDATE_ATTR_WORKSPACE" );

			if(helper.isTypeOfFolder(obj.getType())) {
				// SearchFramework 検索FW更新通知 対象：配下のフォルダ・ドキュメント・ドキュメントリンク
				AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, obj,
						"SEARCHFW_UPDATE_ATTR_CHILD_FOLDER", "SEARCHFW_UPDATE_ATTR_CHILD_DOCUMENT",
						"SEARCHFW_UPDATE_ATTR_CHILD_TAG", "SEARCHFW_UPDATE_ATTR_CHILD_DOCUMENTLINK");
			}
		}

		// ドキュメントのリレーションタイプ取得
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));	// ドキュメント

		// 親リレーションの取得
		List parentRelList = RelationUtils.getParentRelationListByRelType(sess, obj, relType,EIMAccessRole.READ);

		// 親オブジェクトの下位引継ぎ属性を格納したHashSet [key]属性タイプID
		HashSet parentLowAttrSet = new HashSet();
		EIMObject parentObj = null;
		if (parentRelList != null && parentRelList.size() > 0) {

			// 親オブジェクトの取得
			parentObj = ((EIMRelation)parentRelList.get(0)).getParent();

			// 親オブジェクトの下位引継ぎ属性を取得
			long[] parentLowAttrs = AppObjectUtil.getIntAttrs(sess, parentObj, EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR"));	// 下位への引継ぎ属性
			if (parentLowAttrs != null) {
				parentLowAttrSet = longArrayToHashSet(parentLowAttrs);
			}
		}

		// 対象オブジェクトのオブジェクトタイプに設定されている属性タイプ一覧を取得
		List attTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, obj.getType());

		// 対象オブジェクトのオブジェクトタイプに設定の属性タイプ格納用 [key]属性タイプID
		HashSet myselfAttrTypeSet = getAttrTypeSetFromList(attTypeList);

		// AttributeUpdaterItemを格納したHashMap [key]属性タイプID [value]AttributeUpdaterItem
		HashMap itemMap = new HashMap();
		if (updater.getAttributeUpdaterItemList() != null) {
			for (Iterator iter = updater.getAttributeUpdaterItemList().iterator(); iter.hasNext();) {
				AttributeUpdaterItem item = (AttributeUpdaterItem)iter.next();

				if (!myselfAttrTypeSet.contains(new Long(item.getAttTypeId()))) {
					String[] message = {AttributeUtils.getAttributeTypeById(sess, item.getAttTypeId()).getName()};
					// 属性【{0}】の割り当ては解除されているため、設定できません。
					throw new EIMException(sess, "EIM.ERROR.LOGIC.ATTRINFO.CANNOTSET.ATTR", message);

				}
				// 以下はドキュメント・フォルダのときのみチェック
				// ※ 対象オブジェクトがタグの場合は属性値の下位引継ぎを行わないためチェックしない
				else if( helper.isTypeOfDocument(obj.getType()) || helper.isTypeOfFolder(obj.getType()) ) {
					if (item.isNameAllocate() && parentLowAttrSet.contains(new Long(item.getAttTypeId()))){
						// 入力した「名称割当て属性」と、親フォルダ、またはワークスペースの「下位引継ぎ属性」が重複しています。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.ATTRINFO.REPETITION.NAME.ALLOCATION.REPETITION");

					} else if (parentLowAttrSet.contains(new Long(item.getAttTypeId()))) {
						String[] message = {AttributeUtils.getAttributeTypeById(sess, item.getAttTypeId()).getName()};
						// 更新対象の属性【{0}】が、親フォルダ、またはワークスペースの「下位引継ぎ属性」と重複しています。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.ATTRINFO.REPETITION.SUCCESSION.ATTR", message);
					}
					else {
						itemMap.put(new Long(item.getAttTypeId()), item);
					}
				} else {
					itemMap.put(new Long(item.getAttTypeId()), item);
				}
			}
		}

		long nameAllocateAttrTypeId = Integer.MIN_VALUE;		// 名称割当て属性の属性タイプID

		List myselfUpAttrList = new ArrayList();	// 対象オブジェクトの「上位引継ぎ属性」格納用
		HashMap myselfLowAttrMap = new HashMap();	// 対象オブジェクトの「下位引継ぎ属性」格納用マップ [key]属性タイプID [value]属性の値(配列) 値がない場合はnull

		// 元々の下位引継ぎ属性を取得
		long[] baseLowAttrs = AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR"));	// 下位への引継ぎ属性
		HashSet baseLowAttrsSet = longArrayToHashSet(baseLowAttrs);	// 元々の下位引継ぎ属性セット [key]属性タイプID

		for (Iterator iter = attTypeList.iterator(); iter.hasNext();) {
			EIMAttributeType attrType = (EIMAttributeType) iter.next();

			// 以下はドキュメント・フォルダのときのみ実施
			// ※ 対象オブジェクトがタグの場合は属性値の下位引継ぎを行わないため実施しない
			// 属性タイプIDが上位EIMObjectの「下位への引継ぎ属性」と一致
			if ((helper.isTypeOfDocument(obj.getType()) || helper.isTypeOfFolder(obj.getType())) &&
					(parentLowAttrSet.contains(new Long(attrType.getId())))) {

				// 上位EIMObjectの属性値を対象EIMObjectに設定
				switch (attrType.getValueType().getId()) {

					// 文字列型の場合
					case EIMValueType.STRING:

						String strAttrs[] = AppObjectUtil.getStrAttrs(sess, parentObj, attrType.getDefaultName());
						ObjectAttributeUtils.setAttribute(sess, obj, attrType, strAttrs);
						myselfLowAttrMap.put(new Long(attrType.getId()), strAttrs);
						break;

					// 数値型の場合
					case EIMValueType.INTEGER:

						long intAttrs[] = AppObjectUtil.getIntAttrs(sess, parentObj, attrType.getDefaultName());
						ObjectAttributeUtils.setAttribute(sess, obj, attrType, TypeConvertUtils.convertToBuildTypeArray(intAttrs));
						myselfLowAttrMap.put(new Long(attrType.getId()), intAttrs);
						break;

						// double数値型の場合
					case EIMValueType.DOUBLE:

						double doubleAttrs[] = AppObjectUtil.getDoubleAttrs(sess, parentObj, attrType.getDefaultName());
						ObjectAttributeUtils.setAttribute(sess, obj, attrType, doubleAttrs);
						myselfLowAttrMap.put(new Long(attrType.getId()), doubleAttrs);
						break;

					// 日付型の場合
					case EIMValueType.DATE:

						Date dateAttrs[] = AppObjectUtil.getDateAttrs(sess, parentObj, attrType.getDefaultName());
						ObjectAttributeUtils.setAttribute(sess, obj, attrType, dateAttrs);
						myselfLowAttrMap.put(new Long(attrType.getId()), dateAttrs);
						break;

					// テキスト型の場合
					case EIMValueType.TEXT:

						String textAttrs[] = AppObjectUtil.getTextAttrs(sess, parentObj, attrType.getDefaultName());
						ObjectAttributeUtils.setAttribute(sess, obj, attrType, textAttrs);
						myselfLowAttrMap.put(new Long(attrType.getId()), textAttrs);
						break;
				}
				// 対象オブジェクトの上位引継ぎ属性、下位引継ぎ属性に当該属性タイプIDを設定
				myselfUpAttrList.add(new Long(attrType.getId()));
				continue;
			}

			// 属性情報画面での入力値を取得
			AttributeUpdaterItem item = (AttributeUpdaterItem)itemMap.get(new Long(attrType.getId()));

			// 対象オブジェクトの属性に該当する入力値が送られなかった場合
			if (item == null) {

				// しかし、その属性は元々の「下位引継ぎ属性」である場合
				if (baseLowAttrsSet.contains(new Long(attrType.getId()))) {

					// 元々の属性値をそのまま引継ぐべき「下位引継ぎ属性」に設定
					switch (attrType.getValueType().getId()) {

						// 文字列型の場合
						case EIMValueType.STRING:

							String[] strAttrs = AppObjectUtil.getStrAttrs(sess, obj, attrType.getDefaultName());
							myselfLowAttrMap.put(new Long(attrType.getId()), strAttrs);
							break;

						// 数値型の場合
						case EIMValueType.INTEGER:

							long intAttrs[] = AppObjectUtil.getIntAttrs(sess, obj, attrType.getDefaultName());
							myselfLowAttrMap.put(new Long(attrType.getId()), intAttrs);
							break;

							// double数値型の場合
						case EIMValueType.DOUBLE:

							double doubleAttrs[] = AppObjectUtil.getDoubleAttrs(sess, obj, attrType.getDefaultName());
							myselfLowAttrMap.put(new Long(attrType.getId()), doubleAttrs);
							break;

						// 日付型の場合
						case EIMValueType.DATE:

							Date dateAttrs[] = AppObjectUtil.getDateAttrs(sess, obj, attrType.getDefaultName());
							myselfLowAttrMap.put(new Long(attrType.getId()), dateAttrs);
							break;

						// テキスト型の場合
						case EIMValueType.TEXT:

							String[] textAttrs = AppObjectUtil.getTextAttrs(sess, obj, attrType.getDefaultName());
							myselfLowAttrMap.put(new Long(attrType.getId()), textAttrs);
							break;
					}
				}

			// 属性の設定値がない場合
			} else if (item.getAttValues() == null || StringUtils.isBlank((item.getAttValues())[0])) {

				// 当該属性を対象オブジェクトから削除
				ObjectAttributeUtils.deleteAttribute(sess, obj, attrType);

				// 下位引継ぎ属性に指定されている場合
				if (item.isLowerSuccession()) {
					myselfLowAttrMap.put(new Long(attrType.getId()), null);
				}

			} else {
				// リスト属性の場合の登録値チェック
				checkValueIncludeInListDef(sess,attrType,item);

				// 対象オブジェクトの属性値に入力値を設定
				switch (attrType.getValueType().getId()) {

				// 文字列型の場合
				case EIMValueType.STRING:
				// テキスト型の場合
				case EIMValueType.TEXT:

					ObjectAttributeUtils.setAttribute(sess, obj, attrType, item.getAttValues());

					// 下位引継ぎ属性に指定されている場合
					if (item.isLowerSuccession()) {
						myselfLowAttrMap.put(new Long(attrType.getId()), item.getAttValues());
					}
					break;

				// 数値型の場合
				case EIMValueType.INTEGER:

					long intAttrs[] = new long[item.getAttValues().length];
					for (int i = 0 ; i < item.getAttValues().length ; i++ ) {
						intAttrs[i] = Long.parseLong((item.getAttValues())[i]);
					}
					ObjectAttributeUtils.setAttribute(sess, obj, attrType, TypeConvertUtils.convertToBuildTypeArray(intAttrs));

					// 下位引継ぎ属性に指定されている場合
					if (item.isLowerSuccession()) {
						myselfLowAttrMap.put(new Long(attrType.getId()), intAttrs);
					}
					break;

					// double数値型の場合
				case EIMValueType.DOUBLE:

					double doubleAttrs[] = new double[item.getAttValues().length];
					for (int i = 0 ; i < item.getAttValues().length ; i++ ) {
						doubleAttrs[i] = Double.parseDouble((item.getAttValues())[i]);
					}
					ObjectAttributeUtils.setAttribute(sess, obj, attrType, doubleAttrs);

					// 下位引継ぎ属性に指定されている場合
					if (item.isLowerSuccession()) {
						myselfLowAttrMap.put(new Long(attrType.getId()), doubleAttrs);
					}
					break;

				// 日付型の場合
				case EIMValueType.DATE:

					Date dateAttrs[] = new Date[item.getAttValues().length];
					for (int i = 0 ; i < item.getAttValues().length ; i++ ) {
						// GMT変換
						dateAttrs[i] = DateUtils.editExpirationDate(sess, StringUtils.getDateFromString((item.getAttValues())[i],
								EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
					}
					ObjectAttributeUtils.setAttribute(sess, obj, attrType, dateAttrs);

					// 下位引継ぎ属性に指定されている場合
					if (item.isLowerSuccession()) {
						myselfLowAttrMap.put(new Long(attrType.getId()), dateAttrs);
					}
					break;
				}

				// 名称割当て属性に指定されている場合
				if (item.isNameAllocate()) {
					nameAllocateAttrTypeId = attrType.getId();
				}
			}
		}

		// 名称割当て属性が指定されている場合
		if (nameAllocateAttrTypeId != Integer.MIN_VALUE) {
			// 名称割当て属性を設定
			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_NAME_ATTR"), nameAllocateAttrTypeId);	// 名称割当て属性

		// 名称割当て属性が指定されていなかった場合
		} else {
			// 元々の名称割当て属性の取得
			long oldNameAllocateAttrTypeId = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_NAME_ATTR"), Integer.MIN_VALUE);	// 名称割当て属性
			// 元々の名称割当て属性が存在する、かつ、その属性が今回の入力属性として渡されている場合、名称割当て属性を削除する
			if (oldNameAllocateAttrTypeId != Integer.MIN_VALUE
					&& itemMap.containsKey(new Long(oldNameAllocateAttrTypeId))) {
				AppObjectUtil.deleteAttribute(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_NAME_ATTR"));
			}
		}

		// 上位引継ぎ属性が存在する場合
		if (myselfUpAttrList.size() > 0) {
			// 上位引継ぎ属性を設定
			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"), listToLongArray(myselfUpAttrList));	// 上位からの引継ぎ属性

			List aList = parentObj.getAttributeList();
			HashMap aMap = new HashMap();
			for(int ii = 0; ii < aList.size(); ii++) {
				//属性のIDをキーに属性をマップに格納
				aMap.put(new Long(((EIMAttribute)aList.get(ii)).getType().getId()), (EIMAttribute)aList.get(ii));
			}

			List attrTypeList = new ArrayList();
			for(int ii = 0; ii < myselfUpAttrList.size(); ii++) {
				Long inheritId = (Long)myselfUpAttrList.get(ii);
				EIMAttribute a = (EIMAttribute)aMap.get(inheritId);
				//属性値がブランクで下位引継指定されている時用にnullチェック
				if(a != null) {
					attrTypeList.add(a.getType());
				}
			}

			//上位引継ぎの属性表示色を更新
			DisplayColorUtil.inheritDisplayColor(sess, obj, attrTypeList, parentObj);

		} else {
			// 元々の上位引継ぎ属性を削除
			long[] baseHighAttrs = AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"));	// 上位からの引継ぎ属性
			if (baseHighAttrs != null) {
				AppObjectUtil.deleteAttribute(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"));	// 上位からの引継ぎ属性
			}
		}

		// フォルダ、またはワークスペースの場合のみ下位引継ぎ処理を実施
		if (helper.isTypeOfFolder(obj.getType()) || helper.isTypeOfWorkspace(obj.getType())) {

			// 下位引継ぎ属性が存在する場合
			if (myselfLowAttrMap.size() > 0) {
				// 下位引継ぎ属性を設定
				AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR"), mapToLongArray(myselfLowAttrMap));	// 下位への引継ぎ属性
			} else {
				// 元々の下位引継ぎ属性を削除
				if (baseLowAttrs != null) {
					AppObjectUtil.deleteAttribute(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR")); // 下位への引継ぎ属性
				}
			}
			HashMap objTypeMap = new HashMap();	// 処理高速化用オブジェクトタイプ格納マップ　[key]オブジェクトタイプID [value]オブジェクトタイプ
			objTypeMap.put(new Long(obj.getType().getId()), obj.getType());

			// 子オブジェクトのリレーションリストを取得
			List childRelList = RelationUtils.getChildRelationListByRelType(sess, obj, RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT")),EIMAccessRole.NONE);

			if (childRelList != null) {
				for (Iterator iter = childRelList.iterator(); iter.hasNext();) {
					// 子オブジェクトの取得
					EIMObject childObj = ((EIMRelation)iter.next()).getChild();

					// 子オブジェクトの属性を更新/削除します。 (再帰処理)
					updateObjAttr(sess, childObj, MODE_UPDATE, null, myselfLowAttrMap, null,
							Integer.MIN_VALUE, null, Integer.MIN_VALUE, Integer.MIN_VALUE, objTypeMap, null, helper);
				}
			}
		}
	}

	/**
	 * 入力規則がリスト定義の属性の登録値がリスト定義内に存在するかどうかをチェックする。
	 *
	 * @param sess
	 * @param attrType
	 * @param item
	 * @return
	 * @throws Exception
	 */
	private static void checkValueIncludeInListDef(EIMSession sess, EIMAttributeType attrType, AttributeUpdaterItem item) throws Exception {
		AttributeValueMaster attributeValueMaster = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType.getId());

		if (!Objects.isNull(attributeValueMaster)) {
			switch (attrType.getValueType().getId()) {
			// 数値型の場合
			case EIMValueType.INTEGER:
				List<Long> masterLongList = new ArrayList<Long>();
				for (long val: attributeValueMaster.getInts()) {
					masterLongList.add(val);
				}

				long intAttrs[] = new long[item.getAttValues().length];
				for (int i = 0 ; i < item.getAttValues().length ; i++ ) {
					intAttrs[i] = Long.parseLong((item.getAttValues())[i]);
				}

				for (long val: intAttrs) {
					if (!masterLongList.contains(val)) {
						throw new EIMException(sess, "EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL", new Object[]{attrType.getDefaultName(),String.valueOf(val)});
					}
				}
				break;
			// DATE型の場合
			case EIMValueType.DATE:
				Date dateAttrs[] = new Date[item.getAttValues().length];
				for (int i = 0 ; i < item.getAttValues().length ; i++ ) {
					// GMT変換
					dateAttrs[i] = DateUtils.editExpirationDate(sess, StringUtils.getDateFromString((item.getAttValues())[i],
							EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				}

				for (Date val: dateAttrs) {
					if (!Arrays.asList(attributeValueMaster.getDates()).contains(val)) {
						SimpleDateFormat sdf = new SimpleDateFormat(ResourceUtils.getByKey("EIM.FORMAT.DATE"));
						throw new EIMException(sess, "EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL", new Object[]{attrType.getDefaultName(),sdf.format(val)});
					}
				}
				break;
			// DOUBLE型の場合
			case EIMValueType.DOUBLE:
				List<Double> masterDoubleList = new ArrayList<Double>();
				for (double val: attributeValueMaster.getDoubles()) {
					masterDoubleList.add(val);
				}

				double doubleAttrs[] = new double[item.getAttValues().length];
				for (int i = 0 ; i < item.getAttValues().length ; i++ ) {
					doubleAttrs[i] = Double.parseDouble((item.getAttValues())[i]);
				}

				for (double val: doubleAttrs) {
					if (!masterDoubleList.contains(val)) {
						throw new EIMException(sess, "EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL", new Object[]{attrType.getDefaultName(),String.valueOf(val)});
					}
				}
				break;
			// STRING型の場合
			case EIMValueType.STRING:
				for (String val: item.getAttValues()) {
					if (!Arrays.asList(attributeValueMaster.getStrings()).contains(val)) {
						throw new EIMException(sess, "EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL", new Object[]{attrType.getDefaultName(),val});
					}
				}
				break;
			// TEXT型の場合
			case EIMValueType.TEXT:
				for (String val: item.getAttValues()) {
					if (!Arrays.asList(attributeValueMaster.getTexts()).contains(val)) {
						throw new EIMException(sess, "EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL", new Object[]{attrType.getDefaultName(),val});
					}
				}
				break;

			case EIMValueType.CODE:
			case EIMValueType.OBJECT:
			case EIMValueType.USER:
			default:
				break;
			}
		}
	}

	/**
	 * 「切り取り＆貼付け」で移動するEIMObjectの属性情報を更新します。
	 *
	 * <li>対象EIMObjectの上位引継ぎ属性に該当する属性を、対象以下の全EIMObjectから削除します。
	 *    (EIMObjectTypeの違いにより途中で分断されている場合は属性の削除もその階層で止まる)
	 * <li>貼付け先EIMObjectの下位引継ぎ属性で、対象以下の全EIMObjectを更新します。
	 * <li>対象以下の全EIMObjectから上位WFフォルダを更新します。
	 * <li>各オブジェクトの論理削除前のセキュリティを復元します。ただしDBに既に存在しないセキュリティの場合は、systemセキュリティを適用します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 移動する対象のEIMObject
	 * @param nextParentObj 貼付け先のEIMObject
	 * @param path 設定すべきパス
	 * @param inRecycleBox 切り取り対象がごみ箱の中か否か (true:ごみ箱の中/false:ごみ箱の外)
	 * @param conditionHelper 条件判定ヘルパーインスタンス
	 * @throws Exception
	 */
	public static void updateAttributeForMove(EIMSession sess, EIMObject obj, EIMObject nextParentObj, EIMObject baseParentObj,
			String path, boolean inRecycleBox, AppObjectConditionHelper conditionHelper) throws Exception {

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = conditionHelper;
		if (helper == null) {
			helper = new AppObjectConditionHelper(sess);
		}

		/* セキュリティのチェック */

		// 切り取り対象オブジェクトのセキュリティのチェック
		if (obj.getSecurity() != null) {
			if (!helper.authorized(obj,sess.getUser(), EIMAccessRole.UPDATE)) {
				// 切り取り/貼付け権限がありません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
			}
		}

		// 貼付け先オブジェクトのセキュリティのチェック
		if (nextParentObj.getSecurity() != null) {
			if (!SecurityUtils.authorized(sess, nextParentObj, sess.getUser(), EIMAccessRole.CREATE)) {
				// 切り取り/貼付け権限がありません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
			}
		}

		/* ステータスのチェック */

		// 貼付け先のステータス種別ID
		long stsKind = nextParentObj.getStatus() != null ? nextParentObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;

		// 貼付け先の上位WFフォルダのステータスのチェック
		if ((helper.isTypeOfFolderWithWorkflow(nextParentObj)
				|| helper.isTypeOfFolderUnderFolderWithWorkflow(nextParentObj))
					&& stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {	// 上位WFのステータスが「編集中」以外の場合はエラー
			// 切り取り/貼付け権限がありません。
			throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
		}

		// 切り取り元フォルダの上位WFフォルダのステータスのチェック
		if ((helper.isTypeOfFolderWithWorkflow(baseParentObj)
				|| helper.isTypeOfFolderUnderFolderWithWorkflow(baseParentObj))
					&& (baseParentObj.getStatus() != null
							&& baseParentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING)) {	// 上位WFのステータスが「編集中」以外の場合はエラー
			// 切り取り/貼付け権限がありません。
			throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
		}

		/* 構成管理権限のチェック */

		// 切り取り対象がフォルダの場合
		if (helper.isTypeOfFolder(obj.getType())) {

			// ごみ箱内ではない場合
			if (!inRecycleBox) {

				// (1)切り取り元フォルダのフォルダ構成管理権限のチェック
				if (baseParentObj != null) {
					if (!AppSecurityUtils.authorizedLowFolderSecurity(sess, baseParentObj, sess.getUser(), EIMAccessRole.UPDATE)) {
						// 切り取り/貼付け権限がありません。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
					}
				}
			}

			// (2)貼付け先フォルダのフォルダ構成管理権限のチェック
			if (!AppSecurityUtils.authorizedLowFolderSecurity(sess, nextParentObj, sess.getUser(), EIMAccessRole.UPDATE)) {
				// 切り取り/貼付け権限がありません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
			}
		}

		// ごみ箱内ではない場合、削除すべき上位引継ぎ属性の取得
		long[] delAttrs = null;
		if (!inRecycleBox) {
			// 元々の上位引継ぎ属性の取得 → 削除すべき上位引継ぎ属性
			delAttrs = AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"));	// 上位からの引継ぎ属性
		}

		// 下位に引継ぐ属性値のMap [key]属性タイプID [value]属性の値(配列)
		HashMap setLowAttrMap = new HashMap();

		// 貼付け先オブジェクトのオブジェクトタイプに設定されている属性タイプ一覧を取得
		List<EIMAttributeType> attTypeList = helper.getAttributeTypeList(nextParentObj.getType());
		HashMap attrTypeMap = getAttrTypeMapFromList(attTypeList);	// 貼付け先オブジェクトの属性タイプ格納用 [key]属性タイプID [value]属性

		// 貼付け先オブジェクトの下位引継ぎ属性を取得
		long[] lowAttrs = AppObjectUtil.getIntAttrs(sess, nextParentObj, EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR"));	// 下位への引継ぎ属性
		if (lowAttrs != null) {
			for (int i = 0 ; i < lowAttrs.length ; i++) {
				if (attrTypeMap.containsKey(new Long(lowAttrs[i]))) {

					EIMAttributeType attrType = (EIMAttributeType)attrTypeMap.get(new Long(lowAttrs[i]));
					switch (attrType.getValueType().getId()) {

						// 文字列型の場合
						case EIMValueType.STRING:

							String strAttrs[] = AppObjectUtil.getStrAttrs(sess, nextParentObj, attrType.getDefaultName());
							setLowAttrMap.put(new Long(attrType.getId()), strAttrs);
							break;

						// 数値型の場合
						case EIMValueType.INTEGER:

							long intAttrs[] = AppObjectUtil.getIntAttrs(sess, nextParentObj, attrType.getDefaultName());
							setLowAttrMap.put(new Long(attrType.getId()), intAttrs);
							break;

							// 実数値型の場合
						case EIMValueType.DOUBLE:

							double doubleAttrs[] = AppObjectUtil.getDoubleAttrs(sess, nextParentObj, attrType.getDefaultName());
							setLowAttrMap.put(new Long(attrType.getId()), doubleAttrs);
							break;

						// 日付型の場合
						case EIMValueType.DATE:

							Date dateAttrs[] = AppObjectUtil.getDateAttrs(sess, nextParentObj, attrType.getDefaultName());
							setLowAttrMap.put(new Long(attrType.getId()), dateAttrs);
							break;

						// テキスト型の場合
						case EIMValueType.TEXT:

							String textAttrs[] = AppObjectUtil.getTextAttrs(sess, nextParentObj, attrType.getDefaultName());
							setLowAttrMap.put(new Long(attrType.getId()), textAttrs);
							break;
					}
				}
			}
		}

		long upWfFolderId = Integer.MIN_VALUE;		// 貼付け先の上位WFフォルダID
		EIMStatus upWfFolderStatus = null;			// 上位WFフォルダのステータス

		// 上位WFフォルダID、上位WFフォルダのステータス種別を取得
		// 貼付け先がWF付きフォルダの場合
		if (helper.isTypeOfFolderWithWorkflow(nextParentObj)) {
			upWfFolderId = nextParentObj.getId();
			upWfFolderStatus = nextParentObj.getStatus();

		// 貼付け先が一般フォルダの場合
		} else {
			upWfFolderId = AppObjectUtil.getIntAttr(sess, nextParentObj,
					EIMConfig.get("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"), Integer.MIN_VALUE);	// 上位WFフォルダ
			if (upWfFolderId != Integer.MIN_VALUE) {
				EIMObject tmpObj = ObjectUtils.getObjectById(sess, upWfFolderId);
				if (tmpObj != null && SecurityUtils.authorized(sess, tmpObj, sess.getUser(), EIMAccessRole.READ)) {
					upWfFolderStatus = tmpObj.getStatus();
				}
			}
		}

		long nextSecurityId = Integer.MIN_VALUE;	// 下位オブジェクトに適応するセキュリティID

		// フォルダ構成管理権限
		long folderSecId = AppObjectUtil.getIntAttr(sess, nextParentObj, EIMConfig.get("ATTR_NAME_FOLDER_LOW_FOLDER_SEC"), Integer.MIN_VALUE);	// 下位フォルダ管理セキュリティ

		// ごみ箱内ではない場合、または、ドキュメントの場合
		// ※ タグはごみ箱からの復帰がないため、このif文には含めない
		if (!inRecycleBox || helper.isTypeOfDocument(obj.getType())) {
			// 貼付け先のセキュリティを引継ぐ
			nextSecurityId = (nextParentObj.getSecurity() != null ? nextParentObj.getSecurityId() : Integer.MIN_VALUE);
		}

		HashMap securityMap = new HashMap();	// 処理高速化用のEIMSecurity格納マップ [key]セキュリティID [value]EIMSecurity
		// ごみ箱内の場合
		if (inRecycleBox) {
			// 論理削除解除の場合にはセキュリティの復元があり、都度セキュリティの存在確認すると効率が悪いのでMapに格納する
			List secList = SecurityUtils.getSecurityList(sess);
			if (secList != null) {
				for (Iterator iter = secList.iterator(); iter.hasNext();) {
					EIMSecurity sec = (EIMSecurity) iter.next();
					securityMap.put(new Long(sec.getId()), sec);
				}
			}
		}

		HashMap objTypeMap = new HashMap();	// 処理高速化用オブジェクトタイプ格納マップ　[key]オブジェクトタイプID [value]オブジェクトタイプ
		objTypeMap.put(new Long(obj.getType().getId()), obj.getType());

		// 自分および自分以下のオブジェクトに削除すべき上位引継ぎ属性の除去、貼付け先の下位引継ぎ属性を反映 (再帰処理)
		updateObjAttr(sess, obj, (inRecycleBox ? MODE_RETURN : MODE_MOVE), path, setLowAttrMap, delAttrs,
				upWfFolderId, upWfFolderStatus, nextSecurityId, folderSecId, objTypeMap, securityMap, helper);
	}

	/**
	 * 「コピー＆ペースト」で生成されたEIMObjectの属性情報を更新します。
	 *
	 * <li>新規生成したオブジェクトは、ObjectAttributeUtils#inheritAttribute()で属性を継承してから設定して下さい。
	 * <li>元々の上位引継ぎ属性を削除します。
	 * <li>貼付け先の下位引継ぎ属性を反映します。
	 * <li>本メソッドは対象がドキュメントの場合のみを前提とします。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj コピーして新規生成されたEIMObject
	 * @param nextParentObj 貼付け先のEIMObject
	 * @param path 設定すべきパス
	 * @throws Exception
	 */
	public static void updateAttributeForCopy(EIMSession sess, EIMObject obj, EIMObject nextParentObj, String path) throws Exception {

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// 元々の上位引継ぎ属性の取得 → 削除すべき上位引継ぎ属性
		long[] delAttrs = AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"));	// 上位からの引継ぎ属性

		// 下位に引継ぐ属性値のMap [key]属性タイプID [value]属性の値(配列)
		HashMap setLowAttrMap = new HashMap();

		// 貼付け先オブジェクトのオブジェクトタイプに設定されている属性タイプ一覧を取得
		List attTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, nextParentObj.getType());
		HashMap attrTypeMap = getAttrTypeMapFromList(attTypeList);	// 貼付け先オブジェクトの属性タイプ格納用 [key]属性タイプID [value]属性

		// 貼付け先オブジェクトの下位引継ぎ属性を取得
		long[] lowAttrs = AppObjectUtil.getIntAttrs(sess, nextParentObj, EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR"));	// 下位への引継ぎ属性
		if (lowAttrs != null) {
			for (int i = 0 ; i < lowAttrs.length ; i++) {
				if (attrTypeMap.containsKey(new Long(lowAttrs[i]))) {

					EIMAttributeType attrType = (EIMAttributeType)attrTypeMap.get(new Long(lowAttrs[i]));
					switch (attrType.getValueType().getId()) {

						// 文字列型の場合
						case EIMValueType.STRING:

							String strAttrs[] = AppObjectUtil.getStrAttrs(sess, nextParentObj, attrType.getDefaultName());
							setLowAttrMap.put(new Long(attrType.getId()), strAttrs);
							break;

						// 数値型の場合
						case EIMValueType.INTEGER:

							long intAttrs[] = AppObjectUtil.getIntAttrs(sess, nextParentObj, attrType.getDefaultName());
							setLowAttrMap.put(new Long(attrType.getId()), intAttrs);
							break;

							// double数値型の場合
						case EIMValueType.DOUBLE:

							double doubleAttrs[] = AppObjectUtil.getDoubleAttrs(sess, nextParentObj, attrType.getDefaultName());
							setLowAttrMap.put(new Long(attrType.getId()), doubleAttrs);
							break;

						// 日付型の場合
						case EIMValueType.DATE:

							Date dateAttrs[] = AppObjectUtil.getDateAttrs(sess, nextParentObj, attrType.getDefaultName());
							setLowAttrMap.put(new Long(attrType.getId()), dateAttrs);
							break;

						// テキスト型の場合
						case EIMValueType.TEXT:

							String textAttrs[] = AppObjectUtil.getTextAttrs(sess, nextParentObj, attrType.getDefaultName());
							setLowAttrMap.put(new Long(attrType.getId()), textAttrs);
							break;
					}
				}
			}
		}

		long upWfFolderId = Integer.MIN_VALUE;		// 貼付け先の上位WFフォルダID
		EIMStatus upWfFolderStatus = null;			// 上位WFフォルダのステータス

		// 上位WFフォルダID、上位WFフォルダのステータスを取得
		// 貼付け先がWF付きフォルダの場合
		if (helper.isTypeOfFolderWithWorkflow(nextParentObj)) {
			upWfFolderId = nextParentObj.getId();
			upWfFolderStatus = nextParentObj.getStatus();

		// 貼付け先が一般フォルダの場合
		} else {
			upWfFolderId = AppObjectUtil.getIntAttr(sess, nextParentObj,
					EIMConfig.get("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"), Integer.MIN_VALUE);	// 上位WFフォルダ
			if (upWfFolderId != Integer.MIN_VALUE) {
				EIMObject tmpObj = ObjectUtils.getObjectById(sess, upWfFolderId);
				if (tmpObj != null && SecurityUtils.authorized(sess, tmpObj, sess.getUser(), EIMAccessRole.READ)) {
					upWfFolderStatus = tmpObj.getStatus();
				}
			}
		}

		// 貼付け先のセキュリティを引継ぐ
		long nextSecurityId = (nextParentObj.getSecurity() != null ? nextParentObj.getSecurityId() : Integer.MIN_VALUE);

		HashMap objTypeMap = new HashMap();	// 処理高速化用オブジェクトタイプ格納マップ　[key]オブジェクトタイプID [value]オブジェクトタイプ
		objTypeMap.put(new Long(obj.getType().getId()), obj.getType());

		// 自分および自分以下のオブジェクトに削除すべき上位引継ぎ属性の除去、貼付け先の下位引継ぎ属性を反映 (再帰処理)
		updateObjAttr(sess, obj, MODE_COPY, path, setLowAttrMap, delAttrs,
				upWfFolderId, upWfFolderStatus, nextSecurityId, Integer.MIN_VALUE, objTypeMap, null, helper);

		// 元のオブジェクトに設定された継承すべきでない属性を削除
		deleteNonInheritAttribute(sess, obj);
	}

	/**
	 * 非継承の属性を削除します。
	 *
	 * <li>本メソッドは対象がドキュメントの場合のみを前提とします。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を削除するEIMObject
	 * @throws Exception
	 */
	public static void deleteNonInheritAttribute(EIMSession sess, EIMObject obj)throws Exception {
		// 非継承の属性を全て削除する
		for(int i = 0; i < AppConstant.NONINHERIT_ATTRIBUTE_DEFNAME.length; i++){
			AppObjectUtil.deleteAttribute(sess, obj, AppConstant.NONINHERIT_ATTRIBUTE_DEFNAME[i]);
		}
	}

	/**
	 * 「論理削除」で移動するEIMObjectの属性情報を更新します。
	 *
	 * <li>対象EIMObjectの上位引継ぎ属性に該当する属性を、対象以下の全EIMObjectから削除します。
	 *    (EIMObjectTypeの違いにより途中で分断されている場合は属性の削除もその階層で止まる)
	 * <li>対象以下の全EIMObjectから上位WFフォルダを更新します。
	 * <li>対象EIMObjectと削除元の親EIMObjectのセキュリティが異なる場合、前セキュリティを保存します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 論理削除する対象のEIMObject
	 * @param parentObj 対象の親EIMObject
	 * @param conditionHelper 条件判定ヘルパーインスタンス
	 * @throws Exception
	 */
	public static void updateAttributeForDelete(EIMSession sess, EIMObject obj, EIMObject parentObj, EIMObject recycleObj,
			AppObjectConditionHelper conditionHelper) throws Exception {

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = conditionHelper;
		if (helper == null) {
			helper = new AppObjectConditionHelper(sess);
		}

		/* 削除可能か対象のトップに位置するEIMObjectを判定 */

		// セキュリティのチェック
		if (obj.getSecurity() != null) {
			if (!SecurityUtils.authorized(sess, obj, sess.getUser(), EIMAccessRole.DELETE)) {
				// 削除権限がありません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
			}
		}

		// フォルダの場合、フォルダ構成管理権限のチェック
		if (parentObj != null && helper.isTypeOfFolder(obj.getType())) {
			if (!AppSecurityUtils.authorizedLowFolderSecurity(sess, parentObj, sess.getUser(), EIMAccessRole.UPDATE)) {
				// 削除権限がありません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
			}
		}

		// 親オブジェクトのステータス種別ID
		long parentStsKind = AppConstant.STATUS_TYPE_KIND_ID_NONE;
		if (parentObj != null && parentObj.getStatus() != null) {
			parentStsKind = parentObj.getStatus().getType().getKind();
		}

		// 上位WFフォルダが存在する場合
		if (parentObj != null
				&& (helper.isTypeOfFolderWithWorkflow(parentObj)
						|| helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj))) {
			// 上位WFのステータスが「編集中」以外の場合はエラー
			if (parentStsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				// 削除権限がありません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
			}

		// 上位WFフォルダが存在しない、かつ、対象がWFを持つ場合
		} else if (helper.isTypeOfFolderWithWorkflow(obj) || helper.isTypeOfDocumentWithWorkflow(obj)){

			// 対象オブジェクトのステータス種別ID
			long stsKind = obj.getStatus() != null ? obj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;

			// 対象のステータスが「編集中」でも「公開済み」でもない場合はエラー
			if (stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING && stsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
				// 削除できるステータスではありません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.CANT.DELETE.STATUS");
			}
		}

		// 元々の上位引継ぎ属性の取得 → 削除すべき上位引継ぎ属性
		long[] delAttrs = AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"));	// 上位からの引継ぎ属性

		// フォルダ構成管理権限 (論理削除なので削除する)
		int folderSecId = Integer.MIN_VALUE;
		HashMap objTypeMap = new HashMap();	// 処理高速化用オブジェクトタイプ格納マップ　[key]オブジェクトタイプID [value]オブジェクトタイプ
		objTypeMap.put(new Long(obj.getType().getId()), obj.getType());

		// 削除対象オブジェクトからパスを取得し、ワークスペース固有ごみ箱自体のパスを取得
		String path = StringUtils.nullToBlank(AppObjectUtil.getPath(obj));
		String [] strs = path.split("/");
		String wsRecyclePath = "/" + strs[1] + "/" ;
		long secId;

		// ワークスペース内からワークスペース固有ごみ箱への移動
		if (!parentObj.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACERECYCLE"))) {

			// 「ワークスペース固有ごみ箱配下セキュリティ」の取得
			secId = (SecurityUtils.getSecurityByName(sess, EIMConfig.get("SECURITY_NAME_WORKSPACE_RECYCLE_CONTENTS"))).getId();

			// 「パス」属性をワークスペース固有ごみ箱配下のパスに変更
			path = wsRecyclePath +  recycleObj.getName() + "/";

		// ワークスペース固有ごみ箱からシステムのごみ箱への移動
		} else {

			// 「system」セキュリティの取得
			secId = (SecurityUtils.getSecurityByName(sess, EIMConfig.get("SECURITY_NAME_SYSTEM"))).getId();
			// 「パス」属性をシステムのごみ箱配下のパスに変更
			path = "/" + recycleObj.getName() + "/";
		}
		// 自分および自分以下のオブジェクトに削除すべき上位引継ぎ属性の除去、上位WFフォルダの更新、前セキュリティの更新 (再帰処理)
		updateObjAttr(sess, obj, MODE_DELETE, path, new HashMap(), delAttrs,
				Integer.MIN_VALUE, null, secId, folderSecId, objTypeMap, null, helper);

	}

	/**
	 * 再帰的にEIMOBjectの属性を更新/削除する共通処理です。
	 *
	 * <li>本メソッドは再帰的に呼び出されて子EIMObjectを更新します。
	 * <li>下位に引継ぐ属性値のみを引数parentLowAttrMapに設定して下さい。
	 * <li>削除すべき上位引継ぎ属性のみを引数delTargetAttrsに設定して下さい。
	 * <li>本メソッドはcommit/rollbackは実施しません。(呼び出し側で実施して下さい。)
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を更新する対象のオブジェクト
	 * @param mode 起動モード(0:属性情報更新/ 1:移動(カット＆ペースト)/ 2:コピー＆ペースト/ 3:論理削除(ごみ箱へ移動)/ 4:論理削除の解除(ごみ箱からの移動))
	 * @param path 設定すべきパス。未設定の場合はnullを設定すること。
	 * @param parentLowAttrMap 「下位への引継ぎ属性」格納用マップ [key]属性タイプID [value]属性の値(配列)
	 * @param delTargetAttrs 削除すべき「上位からの引継ぎ属性」のint配列
	 * @param upWfFolderId 上位WFフォルダIDを更新する場合のみ設定。未設定の場合はInteger.MIN_VALUEを設定すること。
	 * @param upWfFolderStatus 上位WFフォルダのステータス。未設定の場合はnullを設定すること。
	 * @param secId セキュリティを変更する場合のみ設定。未設定の場合はInteger.MIN_VALUEを設定すること。
	 * @param folderSecId 構成管理セキュリティを設定。
	 * @param objTypeMap 処理高速化用のオブジェクトタイプ格納マップ。 [key]オブジェクトタイプID [value]オブジェクトタイプ
	 * @param securityMap 処理高速化用のEIMSecurityのmap。存在確認用。削除解除モードのみ使用。 [key]セキュリティID [value]EIMSecurity
	 * @param helper 条件判定ヘルパー
	 * @param sessHelper セッションヘルパー
	 * @throws Exception
	 */
	private static void updateObjAttr(EIMSession sess, EIMObject obj, int mode, String path,
			HashMap parentLowAttrMap, long[] delTargetAttrs, long upWfFolderId, EIMStatus upWfFolderStatus,
			long secId, long folderSecId, HashMap objTypeMap, HashMap securityMap, AppObjectConditionHelper helper) throws Exception {

		/* 有効期限、ステータス、ロックユーザのチェック */

		//更新（MODE_UPDATE）時以外の場合は権限チェックが必要。
		//更新時は権限がなくても設定・更新が可能
		if(mode != MODE_UPDATE) {
			// 更新権限がありません。
			if(!helper.authorized(obj, sess.getUser(), EIMAccessRole.UPDATE))
			{
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOUPDATEROLE");
			}
			/* フォルダ構成管理チェック */
			// 下位フォルダ管理セキュリティ取得
			if (folderSecId != Integer.MIN_VALUE) {
				if (!AppSecurityUtils.authorizedLowFolderSecurity(sess, obj, sess.getUser(), EIMAccessRole.UPDATE)) {
					// 作成権限がありません
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
				}
			}
		}

		// 「論理削除」モード
		if (mode == MODE_DELETE) {
			// 論理削除可能か否かを判定 (不可の場合は例外発生)
			checkDeleteEnable(sess, obj, helper);

			// 削除日時を設定
			Date date = new Date();
			AppObjectUtil.setAttrDeleteDate(sess, obj, date, helper);
		}

		// 「コピー＆ペースト」「論理削除解除」モード
		if (mode == MODE_COPY ||mode == MODE_RETURN) {
			if (obj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_DELETE_DATE")) != null) {
				// 削除日時を削除
				AppObjectUtil.deleteAttrDeleteDate(sess, obj, helper);
			}
		}

		/* フォルダ構成管理権限の設定 */

		switch (mode) {
			// 「移動」「論理削除」「論理削除解除」モード
			case MODE_MOVE:
			case MODE_DELETE:
			case MODE_RETURN:

				// フォルダの場合のみ
				if (helper.isTypeOfFolder(obj.getType())) {
					// 上位フォルダ(ワークスペース)の構成管理権限を引継ぐ
					if (folderSecId != Integer.MIN_VALUE) {
						// 構成管理権限の設定
						AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_LOW_FOLDER_SEC"), folderSecId);	// 下位フォルダ管理セキュリティ
					} else {
						// 構成管理権限の解除
						AppObjectUtil.deleteAttribute(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_LOW_FOLDER_SEC"));	// 下位フォルダ管理セキュリティ
					}
				}
				break;
		}

		/* パスの設定 */

		if (mode != MODE_UPDATE) {
			if (path != null) {
				if( mode == MODE_COPY ) {	// コピーの場合は指定パスのみをパス属性に設定する
					AppObjectUtil.setPath(sess, obj, path);	//パス
				}
				else {	// コピー以外の場合はパス属性の先頭だけを入れ替える
					AppObjectUtil.replaceFirstPath(sess, obj, path, helper);	//パス
				}
				path += obj.getName() + "/";

				// フォルダの場合のみ、直下のドキュメントリンクのリンク元ドキュメントのパス属性を更新する
				if (helper.isTypeOfFolder(obj.getType())) {
					AppLogicUtil.renameDocLinkPathUnderFolder(sess, obj, path, helper);
				}
			}
		}

		/* 真の削除すべき上位引継ぎ属性を導出 (「移動」「コピー」「論理削除」モードの場合のみ) */

		List delTargetAttrList = new ArrayList();
		if (mode == MODE_MOVE || mode == MODE_COPY || mode == MODE_DELETE) {
			// 削除すべき上位引継ぎ属性が「下位に引継ぐ属性値」に含まれる場合は除去する (下位引継ぎ属性を残す)
			if (delTargetAttrs != null) {
				for (int i = 0 ; i < delTargetAttrs.length ; i++) {
					if (!parentLowAttrMap.containsKey(new Long(delTargetAttrs[i]))) {
						delTargetAttrList.add(new Long(delTargetAttrs[i]));
					}
				}
			}
		}

		// 真の削除すべき「上位からの引継ぎ属性」のHashSet [key]属性タイプID
		HashSet delTargetAttrSet = longArrayToHashSet(listToLongArray(delTargetAttrList));

		// 当該オブジェクトの元々の「上位からの引継ぎ属性」のHashSet [key]属性タイプID
		HashSet baseHighAttrs = longArrayToHashSet(
				AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_FROM_HIGH_ATTR")));	// 上位からの引継ぎ属性

		// オブジェクトタイプのみ再取得
		// (※リレーションから取得した子オブジェクトには継承した属性タイプが設定されないための回避措置)
		if (!objTypeMap.containsKey(new Long(obj.getType().getId()))) {
			objTypeMap.put(new Long(obj.getType().getId()),
					ObjectUtils.getObjectTypeById(sess, obj.getType().getId()));
		}

		// 対象オブジェクトのオブジェクトタイプが持つ属性タイプを取得
		List<EIMAttributeType> attTypeList = helper.getAttributeTypeList((EIMObjectType) objTypeMap.get(new Long(obj.getType().getId())));

		// 「属性情報更新」モードの場合は、対象属性がなければ更に探索はしない
		if (mode == MODE_UPDATE && (attTypeList == null || attTypeList.size() == 0)) {
				// 属性を持たない場合、下位引継ぎは発生しないので次ループへ
				return;
		} else {
			boolean targetExistFlag = false;	// 引継ぎ属性の存在有無
			// オブジェクトのタイプがフォルダ or ドキュメントのときに確認する
			// ※ タグの場合は属性値の下位引継ぎを行わないため、強制的に「引継ぎ属性なし」とする
			if(helper.isTypeOfDocument(obj.getType()) || helper.isTypeOfFolder(obj.getType())) {
				for (Iterator<EIMAttributeType> iterator = attTypeList.iterator(); iterator.hasNext();) {
					EIMAttributeType attrType = (EIMAttributeType) iterator.next();
					if (parentLowAttrMap.containsKey(new Long(attrType.getId()))){
						targetExistFlag = true;
						break;
					}
				}
			}
			// 親の下位引継ぎ属性に該当する属性を持たない場合、元々の上位引継ぎ属性を削除
			if (!targetExistFlag) {
				if (baseHighAttrs.size() > 0) {
					// 元々の上位引継ぎ属性を削除
					AppObjectUtil.deleteAttribute(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"));	// 上位からの引継ぎ属性
				}

				// 親の下位引継ぎ属性が解除された場合、子フォルダの下位引継ぎ属性も解除する
				if (mode == MODE_UPDATE && helper.isTypeOfFolder(obj.getType()) && baseHighAttrs.size() > 0) {
					// 対象オブジェクトの元々の下位引継ぎ属性を取得する
					long[] baseLowAttrs = AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR"));	// 下位への引継ぎ属性

					// 解除された下位引継ぎ属性を除外する
					List<Long> newLowAttrList = new ArrayList<>();
					for (int i = 0; i < baseLowAttrs.length; i ++) {
						if (!baseHighAttrs.contains(baseLowAttrs[i])) {
							newLowAttrList.add(baseLowAttrs[i]);
						}
					}
					long[] newLowAttrs = newLowAttrList.stream().mapToLong(item -> item.longValue()).toArray();

					// 対象オブジェクトの属性「下位への引継ぎ属性」を更新する
					AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR"), newLowAttrs);	// 下位への引継ぎ属性

					// 対象オブジェクトのリレーションリストを取得
					List<EIMRelation> childRelList = helper.getChildRelationListByRelType(obj, helper.getRelationTypeOfDocument(), EIMAccessRole.NONE);

					// 子オブジェクトの「下位への引継ぎ属性」を再帰更新する
					if (childRelList != null) {
						for (Iterator<EIMRelation> iter = childRelList.iterator(); iter.hasNext();) {
							// 子オブジェクトの取得
							EIMObject childObj = ((EIMRelation)iter.next()).getChild();

							// 本メソッドの再帰呼び出し
							updateObjAttr(sess, childObj, mode, path,
									new HashMap<>(), new long[0], upWfFolderId, upWfFolderStatus,
									secId, folderSecId, objTypeMap, securityMap, helper);
						}
					}
				}

				// 「属性情報更新」モードの場合、次ループへ
				if (mode == MODE_UPDATE) {
					return;
				}
			}
		}

		// 以下の処置はドキュメント・フォルダについてのみ実施する
		// ※ タグは属性を下位に引き継がないため実施しない
		HashMap lowAttrMap = new HashMap();	// 対象オブジェクトの下位引継ぎ属性 格納用マップ [key]属性タイプID [value]属性の値(配列)
		List nextDelTargetAttrList = new ArrayList();	// 次の下位階層での「削除すべき上位引継ぎ属性」のリスト
		List truthParentLowAttrList = new ArrayList();	// 真の「上位からの引継ぎ属性」格納用リスト

		if( helper.isTypeOfDocument(obj.getType()) || helper.isTypeOfFolder(obj.getType()) ) {
			// 対象オブジェクトの名称割当て属性と親オブジェクトの下位引継ぎ属性が重複した場合はエラー
			long nameAllocateId = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_NAME_ATTR"), Integer.MIN_VALUE);	// 名称割当て属性
			if (nameAllocateId != Integer.MIN_VALUE
					&& parentLowAttrMap.containsKey(new Long(nameAllocateId))) {
				String[] message = {(AttributeUtils.getAttributeTypeById(sess, nameAllocateId)).getName(), obj.getName()};
				// 下位引継ぎ属性【{0}】と、【{1}】の「名称割当て属性」が重複しています。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.ATTRINFO.REPETITION.LOWATTR.AND.NAME.ALLOCATION", message);
			}

			/* 対象オブジェクトの属性値取得 */

			HashMap attrMap = new HashMap();	// [key]属性タイプID [value]属性の値(配列)
			List attrList = obj.getAttributeList();
			if (attrList != null) {
				for (Iterator iterator = attrList.iterator(); iterator.hasNext();) {
					EIMAttribute attr = (EIMAttribute) iterator.next();

					// 対象オブジェクトの属性値に入力値を設定
					switch (attr.getType().getValueType().getId()) {

					// 文字列型の場合
					case EIMValueType.STRING:

						attrMap.put(new Long(attr.getType().getId()), attr.getStrings());
						break;

					// 数値型の場合
					case EIMValueType.INTEGER:

						attrMap.put(new Long(attr.getType().getId()), attr.getInts());
						break;

						// double数値型の場合
					case EIMValueType.DOUBLE:

						attrMap.put(new Long(attr.getType().getId()), attr.getDoubles());
						break;

					// 日付型の場合
					case EIMValueType.DATE:

						attrMap.put(new Long(attr.getType().getId()), attr.getDates());
						break;

					// テキスト型の場合
					case EIMValueType.TEXT:

						attrMap.put(new Long(attr.getType().getId()), attr.getTexts());
						break;
					}
				}
			}

			// 対象オブジェクトの元々の下位引継ぎ属性を取得
			long[] baseLowAttrs = AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR"));	// 下位への引継ぎ属性
			if (baseLowAttrs != null) {
				for (int i = 0 ; i < baseLowAttrs.length ; i++) {

					// 真の削除すべき「上位からの引継ぎ属性」の場合は格納しない
					if (delTargetAttrSet.contains(new Long(baseLowAttrs[i]))) {
						continue;
					}

					// 親から引継ぐことになる属性値(＝上位引継ぎ属性)の場合はここではまだ格納しない
					if (!parentLowAttrMap.containsKey(new Long(baseLowAttrs[i]))) {
						// 親の下位引継ぎ属性に含まれず、元々の上位引継ぎ属性に含まれる場合は除外する
						// (つまり親の下位引継ぎ属性が解除された場合は自身の下位引継ぎ属性も解除する)
						if (mode == MODE_UPDATE && baseHighAttrs.contains(new Long(baseLowAttrs[i]))) {
							;
						}
						else {
							// 対象オブジェクトの下位引継ぎ属性に格納
							lowAttrMap.put(new Long(baseLowAttrs[i]), attrMap.get(new Long(baseLowAttrs[i])));
						}
					}
					// 親から引継ぐことになる属性値と一致する、かつ、元々の上位引継ぎ属性に含まれない
					// (つまり自分が起点となる下位引継ぎ属性と上位からの引継ぎ属性がぶつかった)場合はエラー
					else if (!baseHighAttrs.contains(new Long(baseLowAttrs[i]))) {
						EIMAttributeType attrType = AttributeUtils.getAttributeTypeById(sess, baseLowAttrs[i]);
						String[] message = {attrType.getName(), obj.getName()};
						// 下位引継ぎ属性【{0}】が、【{1}】の下位引継ぎ属性と重複しています。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.REPETITION.SUCCESSION.ATTR", message);
					}
				}
			}

			List attrTypeList = new ArrayList();

			for (Iterator iterator = attTypeList.iterator(); iterator.hasNext();) {
				EIMAttributeType attrType = (EIMAttributeType) iterator.next();

				// 対象オブジェクトのオブジェクトタイプが引継ぎ属性を保持する場合
				if (parentLowAttrMap.containsKey(new Long(attrType.getId()))) {

					Object tmpObjs = parentLowAttrMap.get(new Long(attrType.getId()));
					if (tmpObjs == null) {
						// 親の属性値が空の場合は削除
						ObjectAttributeUtils.deleteAttribute(sess, obj, attrType);
					}

					// 対象オブジェクトに上位引継ぎ属性に該当する属性値を設定
					String tmpStr = "";
					switch(attrType.getValueType().getId()) {

						// 文字列型の場合
						case EIMValueType.STRING:
						// テキスト型の場合
						case EIMValueType.TEXT:

							ObjectAttributeUtils.setAttribute(sess, obj, attrType, (String[]) tmpObjs);
							if(tmpObjs != null) {
								tmpStr = ((String[])tmpObjs)[0];
							}
							break;

						// 数値型の場合
						case EIMValueType.INTEGER:

							ObjectAttributeUtils.setAttribute(sess, obj, attrType, TypeConvertUtils.convertToBuildTypeArray((long[]) tmpObjs));
							if(tmpObjs != null) {
								tmpStr = String.valueOf(((long[])tmpObjs)[0]);
							}
							break;

							// 実数型の場合
						case EIMValueType.DOUBLE:

							ObjectAttributeUtils.setAttribute(sess, obj, attrType, (double[]) tmpObjs);
							if(tmpObjs != null) {
								tmpStr = String.valueOf(((double[])tmpObjs)[0]);
							}
							break;

						// 日付型の場合
						case EIMValueType.DATE:

							ObjectAttributeUtils.setAttribute(sess, obj, attrType, (Date[]) tmpObjs);
							if(tmpObjs != null) {
								tmpStr = StringUtils.getDateStringByFormat(((Date[])tmpObjs)[0], EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));
							}
							break;
					}

					if (attrType.isMultiple() == false && tmpObjs != null) {
						if(path == null) {
							//コピーや切取り貼付けではないので、ここで表示色オブジェクトを更新
							DisplayColorUtil.updateDisplayColor(sess, null, obj, attrType, tmpStr);
						}
						else {
							//リスト値表示色オブジェクトを更新する属性をリストに追加
							attrTypeList.add(attrType);
						}
					}

					// 対象オブジェクトの「下位への引継ぎ属性」に格納
					lowAttrMap.put(new Long(attrType.getId()), parentLowAttrMap.get(new Long(attrType.getId())));

					// 対象オブジェクトの「上位からの引継ぎ属性」に格納
					truthParentLowAttrList.add(new Long(attrType.getId()));

				// 「属性情報更新」モード以外の場合
				} else if (mode != MODE_UPDATE) {
					if (delTargetAttrSet.contains(new Long(attrType.getId()))) {

						// 削除すべき上位引継ぎ属性を削除
						ObjectAttributeUtils.deleteAttribute(sess, obj, attrType);

						// 該当する属性タイプを持つ場合、次の下位階層での「削除すべき上位引継ぎ属性」に追加
						nextDelTargetAttrList.add(new Long(attrType.getId()));
					}
				}
			}

			// 上位引継ぎのものは、親フォルダが保持する属性表示色オブジェクトをコピーする
			// ##DBから最新の属性表示色を取得しない
			DisplayColorUtil.inheritDisplayColor(sess, obj, attrTypeList, path, helper);
		}

		// 「属性情報更新」モード以外の場合、属性「上位WFフォルダ」、ステータスを更新する
		if (mode != MODE_UPDATE) {

			// 上位WFフォルダが存在する場合
			if (upWfFolderId != Integer.MIN_VALUE) {

				// 自分自身がWF付きフォルダ、WF付きドキュメントの場合はエラー
				if (helper.isTypeOfFolderWithWorkflow(obj) || helper.isTypeOfDocumentWithWorkflow(obj)) {
					// ワークフロー付きフォルダの下に、ワークフロー付きのフォルダまたはドキュメントは配置できません。
					throw new EIMException(sess, "EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCFOL");

				} else {
					// 属性「上位WFフォルダ」の設定
					AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"), upWfFolderId);	// 上位WFフォルダ

					// ステータスの設定
					if (upWfFolderStatus != null) {
						WorkFlowUtils.updateObjectStatus(sess, obj, upWfFolderStatus);
					}
				}
			// 上位WFが存在しない場合
			} else {

				// 自分自身がWF付きフォルダの場合
				if (helper.isTypeOfFolderWithWorkflow(obj)) {

					// ここから上位WFフォルダのIDを下位に引継ぐ
					upWfFolderId = obj.getId();

					// ここからステータスを下位に引継ぐ
					upWfFolderStatus = obj.getStatus();

				// タグ・WFを持たないフォルダ、またはドキュメントの場合
				} else if (!helper.isTypeOfDocumentWithWorkflow(obj)){
					long[] tmpInts = AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"));	// 上位WFフォルダ
					if (tmpInts != null) {
						// 属性「上位WFフォルダ」の削除
						AppObjectUtil.deleteAttribute(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"));	// 上位WFフォルダ
					}
					if (obj.getStatus() != null) {
						// ステータスの解除
						WorkFlowUtils.updateObjectStatus(sess, obj, null);
					}
				}
			}
		}

		// 親オブジェクトの下位引継ぎ属性を対象オブジェクトの上位引継ぎ属性に設定
		// ※ 対象オブジェクトがタグの場合は実施しない(属性値を下位に引き継がないため)
		if( helper.isTypeOfDocument(obj.getType()) || helper.isTypeOfFolder(obj.getType()) )
			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"), listToLongArray(truthParentLowAttrList));	// 上位からの引継ぎ属性

		// フォルダ、またはワークスペースの場合
		List childRelList = null;
		if (helper.isTypeOfFolder(obj.getType()) || helper.isTypeOfWorkspace(obj.getType())) {

			// 対象オブジェクトの属性「下位への引継ぎ属性」を設定
			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR"), mapToLongArray(lowAttrMap));	// 下位への引継ぎ属性

			// 対象オブジェクトのリレーションリストを取得
			childRelList = helper.getChildRelationListByRelType(obj, helper.getRelationTypeOfDocument(), EIMAccessRole.NONE);
		}

		/* セキュリティの設定 */

		// セキュリティの変更により対象EIMObjectを操作出来なくなる場合があるため、セキュリティは最後尾で設定する
		switch (mode) {
			// 「移動」「コピー」モード
			case MODE_MOVE:
			case MODE_COPY:

				// 指定のセキュリティを設定
				if (secId != Integer.MIN_VALUE) {
					SecurityUtils.setSecurity(sess, obj, new EIMSecurity(secId, null, null));
				}
				break;

			// 「論理削除」モード
			case MODE_DELETE:

				// フォルダの場合のみ、前セキュリティを保存する
				if (obj.getSecurity() != null && helper.isTypeOfFolder(obj.getType())) {
					// ワークスペース固有ごみ箱配下からの論理削除の場合は前セキュリティを変更しない
					// (「/ごみ箱/」から始まる場合はシステムのごみ箱への論理削除時)
					if (!path.startsWith("/" + EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE") + "/")) {
						AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_PREV_SEC"), obj.getSecurityId());	// 前セキュリティ
					}
				}
				// 指定のセキュリティを設定
				if (secId != Integer.MIN_VALUE) {
					SecurityUtils.setSecurity(sess, obj, new EIMSecurity(secId, null, null));
				}
				break;

			// 「論理削除解除」モード
			case MODE_RETURN:

				// フォルダの場合
				if (helper.isTypeOfFolder(obj.getType())) {

					// 前セキュリティの取得
					long beforeSecId = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_PREV_SEC"), Integer.MIN_VALUE);	// 前セキュリティ

					// 前セキュリティを保持する場合
					if (beforeSecId != Integer.MIN_VALUE && securityMap.containsKey(new Long(beforeSecId))) {

						// 前セキュリティを当該オブジェクトのセキュリティに設定
						SecurityUtils.setSecurity(sess, obj, (EIMSecurity)securityMap.get(new Long(beforeSecId)));

						// 属性「前セキュリティ」の削除
						AppObjectUtil.deleteAttribute(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_PREV_SEC"));	// 前セキュリティ

						// 下位ドキュメントには前セキュリティが適応される
						secId = beforeSecId;

					// 前セキュリティを保持しない、または、前セキュリティがもうDBには存在しない場合
					} else {

						// systemセキュリティを設定
						EIMSecurity systemSecurity = SecurityUtils.getSecurityByName(sess, EIMConfig.get("SECURITY_NAME_SYSTEM"));	//system
						SecurityUtils.setSecurity(sess, obj, systemSecurity);

						// 既にDBに存在しない前セキュリティは削除
						if (!securityMap.containsKey(new Long(beforeSecId))) {
							AppObjectUtil.deleteAttribute(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_PREV_SEC"));	// 前セキュリティ
						}

						// 下位ドキュメントにはsystemセキュリティが適応される
						secId = systemSecurity.getId();
					}
				// ドキュメントの場合
				} else {
					// 指定のセキュリティを設定
					if (secId != Integer.MIN_VALUE) {
						SecurityUtils.setSecurity(sess, obj, new EIMSecurity(secId, null, null));
					}
				}
				break;
		}

		// 子オブジェクトへの下位引継ぎ処理
		if (childRelList != null) {
			for (Iterator iter = childRelList.iterator(); iter.hasNext();) {
				// 子オブジェクトの取得
				EIMObject childObj = ((EIMRelation)iter.next()).getChild();

				if (mode == MODE_DELETE && helper.isTypeOfTag(childObj.getType())) {
					// タグの論理削除時は属性更新不要のためcontinue
					continue;
				}
				
				// 本メソッドの再帰呼び出し
				updateObjAttr(sess, childObj, mode, path,
						lowAttrMap, listToLongArray(nextDelTargetAttrList), upWfFolderId, upWfFolderStatus,
						secId, folderSecId, objTypeMap, securityMap, helper);
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
	 * @param sess EIMSessionインスタンス
	 * @param obj 有効期限を判定する対象のEIMObject
	 * @param helper 条件判定ヘルパー
	 * @throws Exception
	 */
	private static void checkDeleteEnable(EIMSession sess, EIMObject obj, AppObjectConditionHelper helper) throws Exception {

		// 有効期限のチェック
		Date effectDate = AppObjectUtil.getDateAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_EFFECT_DATE"));	// 有効期限
		if (effectDate != null) {
			// システム日付 ＜＝ 対象オブジェクトの属性値「有効期限」の場合はエラー
			if (!DateUtils.judgeExpirationDate(sess, effectDate)) {
				// 有効なドキュメント、またはフォルダは削除できません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTDEL.EFFECTIVE.DOCFOL");
			}
		}

		// WFフォルダの場合
		if (helper.isTypeOfFolderWithWorkflow(obj)) {
			long stsKind = obj.getStatus().getType().getKind();

			// ステータスが「編集中」でも「公開済」でもない場合
			if (stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING && stsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC ) {
				// 削除権限がありません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
			}
		}

		// ドキュメントの場合
		if (helper.isTypeOfDocument(obj.getType())) {

			// ロックユーザが存在する場合
			if (obj.getLockUser() != null) {
				// {0}　は、改訂中です。削除できません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETE.REVISING", new Object[]{obj.getName()});
			}

			// WF付きドキュメントの場合
			if (helper.isTypeOfDocumentWithWorkflow(obj)) {
				long stsKind = obj.getStatus().getType().getKind();

				// ステータスが「編集中」でも「公開済」でもない場合
				if (stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING && stsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC ) {
					// {0}　は、承認中です。削除できません。
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETE.APPROVAL", new Object[]{obj.getName()});

				// 編集中かつ履歴番号が0より大きい場合
				} else if(stsKind == AppConstant.STATUS_TYPE_KIND_ID_EDITTING && obj.getRevision() > 0) {
					// {0}　は、改訂中です。削除できません。
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETE.REVISING", new Object[]{obj.getName()});
				}
			// WFなしドキュメント
			} else {
				EIMVersion version = helper.getVersion(obj);
				EIMObject former = version.getObjectByRev( obj.getRevision() - 1 );
				if( former != null && former.getLatest()){
					// {0}　は、改訂中です。削除できません。
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETE.REVISING", new Object[]{obj.getName()});
				}
			}
		}
	}

	/**
	 * Longのリストからlong配列を作成します。
	 *
	 * @param list Integerを格納したリスト
	 * @return int配列
	 */
	private static long[] listToLongArray(List list) {

		long intAttrs[] = new long[list.size()];
		for (int i = 0 ; i < list.size() ; i++ ) {
			intAttrs[i] = ((Long)list.get(i)).longValue();
		}
		return intAttrs;
	}

	/**
	 * long配列からHashSetを作成します。
	 *
	 * @param intAttrs
	 * @return long配列要素をLong型にして格納したHashSet
	 */
	private static HashSet longArrayToHashSet(long[] longAttrs) {

		HashSet set = new HashSet();
		if (longAttrs != null) {
			for (int i = 0 ; i < longAttrs.length ; i++) {
				set.add(new Long(longAttrs[i]));
			}
		}
		return set;
	}

	/**
	 * HashMapからint配列を作成します。
	 *
	 * @param map keyにIntegerを格納したHasmhMap
	 * @return int配列
	 */
	private static long[] mapToLongArray(HashMap map) {

		int i = 0;
		long intAttrs[] = new long[map.size()];
		for (Iterator iter = map.keySet().iterator(); iter.hasNext();) {
			intAttrs[i++] = ((Long)iter.next()).longValue();
		}
		return intAttrs;
	}

	/**
	 * 属性タイプのリストから属性タイプのHashMapを生成します。
	 *
	 * @param attTypeList 属性タイプのリスト
	 * @return 属性タイプID格納Set [key]属性タイプID [value]属性タイプ
	 * @throws Exception
	 */
	private static HashMap getAttrTypeMapFromList(List attTypeList) throws Exception {

		HashMap map = new HashMap();
		if (attTypeList != null) {
			for (Iterator iter = attTypeList.iterator(); iter.hasNext();) {
				EIMAttributeType attrType = (EIMAttributeType) iter.next();
				map.put(new Long(attrType.getId()), attrType);
			}
		}
		return map;
	}

	/**
	 * 属性タイプのリストから属性タイプIDをHashSetを生成します。
	 *
	 * @param attTypeList 属性タイプのリスト
	 * @return 属性タイプID格納Set [key]属性タイプID
	 * @throws Exception
	 */
	private static HashSet getAttrTypeSetFromList(List attTypeList) throws Exception {

		HashSet set = new HashSet();
		if (attTypeList != null) {
			for (Iterator iter = attTypeList.iterator(); iter.hasNext();) {
				EIMAttributeType attrType = (EIMAttributeType) iter.next();
				set.add(new Long(attrType.getId()));
			}
		}
		return set;
	}

	/**
	 * 属性タイプのデフォルト値を指定の値で更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param attType 属性タイプ
	 * @param defaultValueList	デフォルト値のリスト(型に関わらずStringで格納)
	 * @return なし
	 * @throws Exception
	 */
	public static void setDefaultValueApp(EIMSession sess, EIMAttributeType attType, List<String> defaultValueList) throws Exception
	{
		if (defaultValueList == null || defaultValueList.size() == 0 ) {
			// ない場合
			AttributeUtils.deleteDefaultValue(sess, attType);
		}
		else
		{
			switch (attType.getValueType().getId()) {
				// 数値型
				case EIMValueType.INTEGER:
					long[] intValueList = new long[defaultValueList.size()];

					for(int j=0; j<defaultValueList.size(); j++)
					{
						intValueList[j] = Integer.parseInt(defaultValueList.get(j).toString());
					}
					AttributeUtils.setDefaultValue(sess, attType, TypeConvertUtils.convertToBuildTypeArray(intValueList));
					break;

				// 文字列型とテキスト型
				case EIMValueType.STRING:
				case EIMValueType.TEXT:
					String[] strValueList = (String[])defaultValueList.toArray(new String[0]);
					AttributeUtils.setDefaultValue(sess, attType, strValueList);
					break;

				// 日付型
				case EIMValueType.DATE:
					// 既に登録済みの値で、ログインユーザが当該属性値を変更していない場合でも、画面の表示値を新たな入力値として、
					// ログインユーザのクライアント日時からDBサーバ日時に変換した上でDB更新する。
					Date[] dateValueList = new Date[defaultValueList.size()];
					for(int j=0; j<defaultValueList.size(); j++)
					{
						// GMT変換
						Date a = StringUtils.getDateFromString(defaultValueList.get(j).toString(), EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));
						dateValueList[j] = DateUtils.editExpirationDate(sess, a);

					}
					AttributeUtils.setDefaultValue(sess, attType, dateValueList);
					break;

				// ダブル型
				case EIMValueType.DOUBLE:
					double[] doubleValueList = new double[defaultValueList.size()];
					for(int j=0; j<defaultValueList.size(); j++)
					{
						doubleValueList[j] = Double.parseDouble(defaultValueList.get(j).toString());
					}
					AttributeUtils.setDefaultValue(sess, attType, doubleValueList);
					break;
			}
		}
	}

}