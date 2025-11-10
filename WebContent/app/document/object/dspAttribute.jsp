<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "common.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//Parameter
	String prmObjId = request.getParameter("objId");
	String prmObjTypeId = request.getParameter("objTypeId");
	String prmParentObjId = request.getParameter("parentObjId");
	String prmObjTypeDefName = request.getParameter("objTypeDefName");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"objTypeId" + prmObjTypeId,
			"parentObjId" + prmParentObjId,
			"objTypeDefName" + prmObjTypeDefName
			};

	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		user = (EIMUser)sess.getAttribute("USER");


		// マルチインスタンス暫定対応
		String attrTypeDisplayTopmenu = null;
		try{
			attrTypeDisplayTopmenu = EIMConfig.getValue("ATTR_NAME_DISPLAY_TOPMENU");
		}catch(EIMException e){
			// 標準品単体ではコンフィグキーが見つからないエラーが発生するはずなので、そのままスルー
		}

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		if (prmObjId == null || prmObjId.length() < 1) {

			//新規作成関連の画面から遷移した場合
			long objTypeId = Integer.MIN_VALUE;


			objTypeId = Long.parseLong(prmObjTypeId);

			EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, objTypeId);

			//Attribute Types
			List attTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, objType);

			//親オブジェクトの下位引継属性格納用 [key]属性タイプID(String) [value]""
			Map parentLowAttrIdMap = new HashMap();

			EIMObject parentObj = null;

			if (prmParentObjId != null && prmParentObjId.length() > 0) {
				//親オブジェクト取得
				parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmParentObjId));
				if(!SecurityUtils.authorized(sess, parentObj, sess.getUser(),EIMAccessRole.READ)){
					
					message = EIMResource.getMessage(request, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
					log.warn(AppMessageUtils.makeLogMessage(message));
					return;
				}
				//オブジェクトタイプがドキュメント・フォルダの場合は親オブジェクトの下位引継属性を取得する
				//(タグは上位からの属性値引継ぎを行わないため取得しない)
				if( helper.isTypeOfDocument(objType) || helper.isTypeOfFolder(objType) ) {
					long[] parentLowAttrs = AppObjectUtil.getIntAttrs(sess,
							parentObj, EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR"));	// 下位への引継ぎ属性
					if (parentLowAttrs != null) {
						for (int i = 0 ; i < parentLowAttrs.length ; i++) {

							parentLowAttrIdMap.put(String.valueOf(parentLowAttrs[i]), "");

						}
					}
				}
			}

			//Root Node 
			out.println("<attList>");

			for(int i = 0; i < attTypeList.size(); i++)
			{
				EIMAttributeType attType = (EIMAttributeType)attTypeList.get(i);
				
				String attrName = attType.getDefName();
				if (attrName.equals(EIMConfig.get("ATTR_NAME_DOCUMENT_BELONGING_WS"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_DOCUMENT_LINK_BELONGING_WS"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_DOCUMENT_TYPE_FLAG"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_DOCUMENT_TYPE"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_FOLDER_TYPE_FLAG"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_FOLDER_TYPE"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_TAG_TYPE_FLAG"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_TAG_TYPE"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_SECURITY_FLAG"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_SECURITY"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR_TYPE"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_PROCESS_STATUS"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_RESULT_STATUS"))
						|| attrName.equals(EIMConfig.get("ATTR_NAME_DOCUMENT_SEARCH_INDEX")))
				{
					continue;
				}
				
				if(attrTypeDisplayTopmenu != null && attrName.equals(attrTypeDisplayTopmenu)){
					continue;
				}
				
				AttributeValueMaster attrValueMst = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess, attType.getId());
				String value = "";
				StringBuffer attMstListStr = new StringBuffer();
				StringBuffer attValueListStr = new StringBuffer();

				//リスト1件目の空のカラム(未選択用)
				attMstListStr.append(" <attListValue attValue=\"\"/>");

				boolean isUpperSuccession = (parentLowAttrIdMap.get(String.valueOf(attType.getId())) != null);

				String[] colorGetValues = null;
				long[] settingValues = null;

				switch (attType.getValueType().getId()) {
					// 数値型
					case EIMValueType.INTEGER:
						if (attrValueMst != null) {
							long[] masterInts = attrValueMst.getInts();
							colorGetValues = attrValueMst.getColors();
							settingValues = attrValueMst.getSettings();
							for (int j = 0; j < masterInts.length; j++) {
								attMstListStr.append(
										" <attListValue attValue=\"" + masterInts[j] +
										"\" isDspColor=\"" + (settingValues[j] == 1? "true":"false") +
										"\" color=\"" + colorGetValues[j] + "\"/>");
							}
						}

						if (isUpperSuccession) {
							EIMAttribute att = parentObj.getAttribute(attType.getDefName());

							// [09/03/09 modified by ik.] null チェック追加
							if (att != null && att.getInts() != null && att.getInts().length > 0) {
								value = String.valueOf(att.getInts()[0]);

								long[] valueInts = TypeConvertUtils.convertToLongArray(att.getInts());
								for (int j = 0; j < valueInts.length; j++) {
									attValueListStr.append(" <attMultiple attValue=\"" + valueInts[j] + "\"/>");
								}
							}
						}

						break;
					// 文字列型
					case EIMValueType.STRING:
						if (attrValueMst != null) {
							String[] masterStrings = attrValueMst.getStrings();
							colorGetValues = attrValueMst.getColors();
							settingValues = attrValueMst.getSettings();
							for (int j = 0; j < masterStrings.length; j++) {
								attMstListStr.append(
										" <attListValue attValue=\"" + StringUtils.xmlEncode(masterStrings[j]) +
										"\" isDspColor=\"" + (settingValues[j] == 1? "true":"false") +
										"\" color=\"" + colorGetValues[j] + "\"/>");
							}
						}

						if (isUpperSuccession) {
							EIMAttribute att = parentObj.getAttribute(attType.getDefName());

							// [09/03/09 modified by ik.] null チェック追加
							if (att != null && att.getStrings() != null && att.getStrings().length > 0) {
								value = att.getStrings()[0];

								String[] valueStrings = att.getStrings();
								for (int j = 0; j < valueStrings.length; j++) {
									attValueListStr.append(" <attMultiple attValue=\"" + StringUtils.xmlEncode(valueStrings[j]) + "\"/>");
								}
							}
						}

						break;
					// 日付型
					case EIMValueType.DATE:
						if (attrValueMst != null) {
							// タイムゾーン対応したlong型の値を取得
							Date[] masterDates = attrValueMst.getDates();
							colorGetValues = attrValueMst.getColors();
							settingValues = attrValueMst.getSettings();
							for (int j = 0; j < masterDates.length; j++) {
								attMstListStr.append(
										" <attListValue attValue=\"" + DateUtils.getDBTzToCLTzDate(sess, masterDates[j]) +
										"\" isDspColor=\"" + (settingValues[j] == 1? "true":"false") +
										"\" color=\"" + colorGetValues[j] + "\"/>");
							}
						}

						if (isUpperSuccession) {
							EIMAttribute att = parentObj.getAttribute(attType.getDefName());

							// [09/03/09 modified by ik.] null チェック追加
							if (att != null && att.getDates() != null && att.getDates().length > 0) {
								value = DateUtils.getDBTzToCLTzDate(sess, att.getDates()[0]);

								Date[] valueDates = att.getDates();
								for (int j = 0; j < valueDates.length; j++) {
									attValueListStr.append(" <attMultiple attValue=\""
											+ DateUtils.getDBTzToCLTzDate(sess, valueDates[j]) + "\"/>");
								}
							}
						}

						break;
					// テキスト型
					case EIMValueType.TEXT:
						if (attrValueMst != null) {
							String[] masterTexts = attrValueMst.getTexts();
							colorGetValues = attrValueMst.getColors();
							settingValues = attrValueMst.getSettings();
							for (int j = 0; j < masterTexts.length; j++) {
								attMstListStr.append(
										" <attListValue attValue=\"" + StringUtils.xmlEncode(masterTexts[j]) +
										"\" isDspColor=\"" + (settingValues[j] == 1? "true":"false") +
										"\" color=\"" + colorGetValues[j] + "\"/>");
							}
						}

						if (isUpperSuccession) {
							EIMAttribute att = parentObj.getAttribute(attType.getDefName());

							// [09/03/09 modified by ik.] null チェック追加
							if (att != null && att.getTexts() != null && att.getTexts().length > 0) {
								value = att.getTexts()[0];

								String[] valueTexts = att.getTexts();
								for (int j = 0; j < valueTexts.length; j++) {
									attValueListStr.append(" <attMultiple attValue=\"" + StringUtils.xmlEncode(valueTexts[j]) + "\"/>");
								}
							}
						}

						break;

						// ダブル型
					case EIMValueType.DOUBLE:
						if (attrValueMst != null) {
							double[] masterDoubles = attrValueMst.getDoubles();
							colorGetValues = attrValueMst.getColors();
							settingValues = attrValueMst.getSettings();
							for (int j = 0; j < masterDoubles.length; j++) {
								attMstListStr.append(
										" <attListValue attValue=\"" + FormatUtil.getDoubleFormatedString(masterDoubles[j]) +
										"\" isDspColor=\"" + (settingValues[j] == 1? "true":"false") +
										"\" color=\"" + colorGetValues[j] + "\"/>");
							}
						}

						if (isUpperSuccession) {
							EIMAttribute att = parentObj.getAttribute(attType.getDefName());

							// [09/03/09 modified by ik.] null チェック追加
							if (att != null && att.getDoubles() != null && att.getDoubles().length > 0) {
								value = String.valueOf(att.getDoubles()[0]);

								double[] valueDoubles = att.getDoubles();
								for (int j = 0; j < valueDoubles.length; j++) {
									attValueListStr.append(" <attMultiple attValue=\"" + FormatUtil.getDoubleFormatedString(valueDoubles[j]) + "\"/>");
								}
							}
						}

						break;
				}

				/* [09/03/04 modified by ik.]
				 * 「リスト1件目の空のカラム(未選択用)」付近から移動。
				 *  → 新規フォルダ作成画面から属性画面を表示した際に入力欄が余分に表示されるのを防ぐため。
				 */
				//複数値属性の入力欄が[追加]ボタンを押下してからでないと入力できないこと防止の為、空のカラム追加
				if (attValueListStr.length() == 0) {
					attValueListStr.append(" <attMultiple attValue=\"\"/>");
				}

				EIMObject attrMstObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"), String.valueOf(attType.getId()));

				//XML Out
				out.println("<attribute");
					out.println(" attTypeId=\"" + attType.getId() + "\"");
					out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
					out.println(" attTypeDefName=\"" + StringUtils.xmlEncode(attType.getDefName()) + "\"");
					out.println(" attValue=\"" + StringUtils.xmlEncode(value) + "\"");
					out.println(" valTypeId=\"" + attType.getValueType().getId() + "\"");
					out.println(" valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");

					out.println(" isNameAllocate=\"" + "false" + "\"");
					out.println(" isUpperSuccession=\"" + isUpperSuccession + "\"");
					out.println(" isLowerSuccession=\"" + "false" + "\"");
					out.println(" isListValue=\"" + (attrMstObj != null) + "\"");
					out.println(" isMultiple=\"" + (attType.isMultiple()) + "\"");
				out.println(">");
					out.println(" <attListValueList>");
						out.println(attMstListStr.toString());
					out.println(" </attListValueList>");
					out.println(" <attMultipleList>");
						out.println(attValueListStr.toString());
					out.println(" </attMultipleList>");

				out.println("</attribute>");
			}

			if (objType.getDefaultName().equals(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"))) {

				//有効期限が存在しないためダミーのXMLを追加
				EIMAttributeType dateObjType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_EFFECT_DATE"));
				//XML Out
				out.println("<attribute");
					out.println(" attTypeId=\"" + dateObjType.getId() + "\"");
					out.println(" attTypeName=\"" + StringUtils.xmlEncode(dateObjType.getName()) + "\"");
					out.println(" attTypeDefName=\"" + StringUtils.xmlEncode(dateObjType.getDefName()) + "\"");
					out.println(" attValue=\"\"");
					out.println(" valTypeId=\"" + dateObjType.getValueType().getId() + "\"");
					out.println(" valTypeName=\"" + StringUtils.xmlEncode(dateObjType.getValueType().getName()) + "\"");

					out.println(" isNameAllocate=\"" + "false" + "\"");
					out.println(" isUpperSuccession=\"" + "false" + "\"");
					out.println(" isLowerSuccession=\"" + "false" + "\"");
					out.println(" isListValue=\"" + "false" + "\"");
					out.println(" isMultiple=\"" + "false" + "\"");
				out.println(">");
				out.println("</attribute>");

			}

			//End Root Node
			out.println("</attList>");

			return;

		}

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null || !SecurityUtils.authorized(sess, object, sess.getUser(),EIMAccessRole.READ))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		//Object Type For Parent
		EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());

		//Attribute Types
		List attTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, objType);

		//Root Node
		out.println("<attList>");

		//名称割当て属性
		EIMAttribute nameAllocateAttr = object.getAttribute(EIMConfig.get("ATTR_NAME_FOLDER_NAME_ATTR"));
		//上位からの引継ぎ属性
		EIMAttribute upperSuccessionAttr = object.getAttribute(EIMConfig.get("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"));
		//下位への引継ぎ属性
		EIMAttribute lowerSuccessionAttr = object.getAttribute(EIMConfig.get("ATTR_NAME_FOLDER_TO_LOW_ATTR"));

		for(int i = 0; i < attTypeList.size(); i++)
		{
			EIMAttributeType attType = (EIMAttributeType)attTypeList.get(i);
			
			String attrName = attType.getDefName();
			if (attrName.equals(EIMConfig.get("ATTR_NAME_DOCUMENT_BELONGING_WS"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_DOCUMENT_LINK_BELONGING_WS"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_DOCUMENT_TYPE_FLAG"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_DOCUMENT_TYPE"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_FOLDER_TYPE_FLAG"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_FOLDER_TYPE"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_TAG_TYPE_FLAG"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_TAG_TYPE"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_SECURITY_FLAG"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_SECURITY"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR_TYPE"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_PROCESS_STATUS"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_RESULT_STATUS"))
					|| attrName.equals(EIMConfig.get("ATTR_NAME_DOCUMENT_SEARCH_INDEX")))
			{
				continue;
			}
			
			if(attrTypeDisplayTopmenu != null && attrName.equals(attrTypeDisplayTopmenu)){
				continue;
			}
			
			EIMAttribute att = object.getAttribute(attType.getDefName());
			String value = "";
			StringBuffer attMstListStr = new StringBuffer();
			StringBuffer attValueListStr = new StringBuffer();
			AttributeValueMaster attrValueMst = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess, attType.getId());

			//リスト1件目の空のカラム(未選択用)
			attMstListStr.append(" <attListValue attValue=\"\"/>");

			String[] colorGetValues = null;
			long[] settingValues = null;

			switch (attType.getValueType().getId()) {
				// 数値型
				case EIMValueType.INTEGER:
					if (attrValueMst != null) {
						try {
							long[] masterInts = attrValueMst.getInts();
							colorGetValues = attrValueMst.getColors();
							settingValues = attrValueMst.getSettings();

							for (int j = 0; j < masterInts.length; j++) {
								attMstListStr.append(
										" <attListValue attValue=\"" + masterInts[j] +
										"\" isDspColor=\"" + (settingValues[j] == 1? "true":"false") +
										"\" color=\"" + colorGetValues[j] + "\"/>");
							}
						} catch (EIMException eime) {
							// リスト値がない場合は何もしない
							if (!eime.getMessageKey().equals("EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE")) {
								throw eime;
							}
						}
					}

					if (att != null && att.getInts() != null && att.getInts().length > 0) {
						value = String.valueOf(att.getInts()[0]);

						long[] valueInts = TypeConvertUtils.convertToLongArray(att.getInts());
						for (int j = 0; j < valueInts.length; j++) {
							attValueListStr.append(" <attMultiple attValue=\"" + valueInts[j] + "\"/>");
						}
					}

					break;
				// 文字列型
				case EIMValueType.STRING:
					if (attrValueMst != null) {
						try {
							String[] masterStrings = attrValueMst.getStrings();
							colorGetValues = attrValueMst.getColors();
							settingValues = attrValueMst.getSettings();
							for (int j = 0; j < masterStrings.length; j++) {
								attMstListStr.append(
										" <attListValue attValue=\"" + StringUtils.xmlEncode(masterStrings[j]) +
										"\" isDspColor=\"" + (settingValues[j] == 1? "true":"false") +
										"\" color=\"" + colorGetValues[j] + "\"/>");
							}
						} catch (EIMException eime) {
							// リスト値がない場合は何もしない
							if (!eime.getMessageKey().equals("EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE")) {
								throw eime;
							}
						}
					}

					if (att != null && att.getStrings() != null && att.getStrings().length > 0) {
						value = att.getStrings()[0];

						String[] valueStrings = att.getStrings();
						for (int j = 0; j < valueStrings.length; j++) {
							attValueListStr.append(" <attMultiple attValue=\"" + StringUtils.xmlEncode(valueStrings[j]) + "\"/>");
						}
					}

					break;
				// 日付型
				case EIMValueType.DATE:
					if (attrValueMst != null) {
						try {
							// タイムゾーン対応したlong型の値を取得
							Date[] masterDates = attrValueMst.getDates();
							colorGetValues = attrValueMst.getColors();
							settingValues = attrValueMst.getSettings();
							for (int j = 0; j < masterDates.length; j++) {
								attMstListStr.append(
										" <attListValue attValue=\"" + DateUtils.getDBTzToCLTzDate(sess, masterDates[j]) +
										"\" isDspColor=\"" + (settingValues[j] == 1? "true":"false") +
										"\" color=\"" + colorGetValues[j] + "\"/>");
							}
						} catch (EIMException eime) {
							// リスト値がない場合は何もしない
							if (!eime.getMessageKey().equals("EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE")) {
								throw eime;
							}
						}
					}

					if (att != null && att.getDates() != null && att.getDates().length > 0) {
						value = DateUtils.getDBTzToCLTzDate(sess, att.getDates()[0]);

						Date[] valueDates = att.getDates();
						for (int j = 0; j < valueDates.length; j++) {
							attValueListStr.append(" <attMultiple attValue=\""
									+ DateUtils.getDBTzToCLTzDate(sess, valueDates[j]) + "\"/>");
						}
					}

					break;
				// テキスト型
				case EIMValueType.TEXT:
					if (attrValueMst != null) {
						try {
							String[] masterTexts = attrValueMst.getTexts();
							colorGetValues = attrValueMst.getColors();
							settingValues = attrValueMst.getSettings();
							for (int j = 0; j < masterTexts.length; j++) {
								attMstListStr.append(
										" <attListValue attValue=\"" + StringUtils.xmlEncode(masterTexts[j]) +
										"\" isDspColor=\"" + (settingValues[j] == 1? "true":"false") +
										"\" color=\"" + colorGetValues[j] + "\"/>");
							}
						} catch (EIMException eime) {
							// リスト値がない場合は何もしない
							if (!eime.getMessageKey().equals("EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE")) {
								throw eime;
							}
						}
					}

					if (att != null && att.getTexts() != null && att.getTexts().length > 0) {
						value = att.getTexts()[0];

						String[] valueTexts = att.getTexts();
						for (int j = 0; j < valueTexts.length; j++) {
							attValueListStr.append(" <attMultiple attValue=\"" + StringUtils.xmlEncode(valueTexts[j]) + "\"/>");
						}
					}

					break;
					// ダブル型
				case EIMValueType.DOUBLE:
					if (attrValueMst != null) {
						try {
							double[] masterDoubles = attrValueMst.getDoubles();
							colorGetValues = attrValueMst.getColors();
							settingValues = attrValueMst.getSettings();

							for (int j = 0; j < masterDoubles.length; j++) {
								attMstListStr.append(
										" <attListValue attValue=\"" + FormatUtil.getDoubleFormatedString(masterDoubles[j]) +
										"\" isDspColor=\"" + (settingValues[j] == 1? "true":"false") +
										"\" color=\"" + colorGetValues[j] + "\"/>");
							}
						} catch (EIMException eime) {
							// リスト値がない場合は何もしない
							if (!eime.getMessageKey().equals("EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE")) {
								throw eime;
							}
						}
					}

					if (att != null && att.getDoubles() != null && att.getDoubles().length > 0) {
						value = FormatUtil.getDoubleFormatedString(att.getDoubles()[0]);

						double[] valueDoubles = att.getDoubles();
						for (int j = 0; j < valueDoubles.length; j++) {
							attValueListStr.append(" <attMultiple attValue=\"" + FormatUtil.getDoubleFormatedString(valueDoubles[j]) + "\"/>");
						}
					}

					break;
			}
			EIMObject attrMstObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"), String.valueOf(attType.getId()));
			if (attValueListStr.length() == 0) {
				attValueListStr.append(" <attMultiple attValue=\"\"/>");
			}

			//名称割当属性ON/OFF判定
			boolean isNameAllocateCheck = false;
			if (nameAllocateAttr != null && nameAllocateAttr.getInts() != null) {
				long[] intArray = TypeConvertUtils.convertToLongArray(nameAllocateAttr.getInts());
				for (int k = 0; k < intArray.length; k++) {
					if (intArray[k] == attType.getId())
						isNameAllocateCheck = true;
				}
			}

			//上位からの引継ぎ属性ON/OFF判定
			boolean isUpperSuccessionCheck = false;
			if (upperSuccessionAttr != null && upperSuccessionAttr.getInts() != null) {
				long[] intArray = TypeConvertUtils.convertToLongArray(upperSuccessionAttr.getInts());
				for (int k = 0; k < intArray.length; k++) {
					if (intArray[k] == attType.getId())
						isUpperSuccessionCheck = true;
				}
			}

			//下位引継ぎ属性ON/OFF判定
			boolean isLowerSuccessionCheck = false;
			if (lowerSuccessionAttr != null && lowerSuccessionAttr.getInts() != null) {
				long[] intArray = TypeConvertUtils.convertToLongArray(lowerSuccessionAttr.getInts());
				for (int k = 0; k < intArray.length; k++) {
					if (intArray[k] == attType.getId())
						isLowerSuccessionCheck = true;
				}
			}

			//XML Out
			out.println("<attribute");
				out.println(" attTypeId=\"" + attType.getId() + "\"");
				out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
				out.println(" attTypeDefName=\"" + StringUtils.xmlEncode(attType.getDefName()) + "\"");
				out.println(" attValue=\"" + StringUtils.xmlEncode(value) + "\"");
				out.println(" valTypeId=\"" + attType.getValueType().getId() + "\"");
				out.println(" valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");

				out.println(" isNameAllocate=\"" + isNameAllocateCheck + "\"");
				out.println(" isUpperSuccession=\"" + isUpperSuccessionCheck + "\"");
				out.println(" isLowerSuccession=\"" + isLowerSuccessionCheck + "\"");
				out.println(" isListValue=\"" + (attrMstObj != null) + "\"");
				out.println(" isMultiple=\"" + (attType.isMultiple()) + "\"");
			out.println(">");
				out.println(" <attListValueList>");
					out.println(attMstListStr.toString());
				out.println(" </attListValueList>");
				out.println(" <attMultipleList>");
					out.println(attValueListStr.toString());
				out.println(" </attMultipleList>");

			out.println("</attribute>");
		}

		if (objType.getDefaultName().equals(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"))) {

			//有効期限が存在しないためダミーのXMLを追加
			EIMAttributeType dateObjType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_EFFECT_DATE"));
			//XML Out
			out.println("<attribute");
				out.println(" attTypeId=\"" + dateObjType.getId() + "\"");
				out.println(" attTypeName=\"" + StringUtils.xmlEncode(dateObjType.getName()) + "\"");
				out.println(" attTypeDefName=\"" + StringUtils.xmlEncode(dateObjType.getDefName()) + "\"");
				out.println(" attValue=\"\"");
				out.println(" valTypeId=\"" + dateObjType.getValueType().getId() + "\"");
				out.println(" valTypeName=\"" + StringUtils.xmlEncode(dateObjType.getValueType().getName()) + "\"");

				out.println(" isNameAllocate=\"" + "false" + "\"");
				out.println(" isUpperSuccession=\"" + "false" + "\"");
				out.println(" isLowerSuccession=\"" + "false" + "\"");
				out.println(" isListValue=\"" + "false" + "\"");
				out.println(" isMultiple=\"" + "false" + "\"");
			out.println(">");
			out.println("</attribute>");

		}

		//End Root Node
		out.println("</attList>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally
	{
		try{
			if(sess != null){
				sess.close();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>