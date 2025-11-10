<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.exception.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	Log log = LogFactory.getLog(this.getClass().getName());

	// ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	EIMSession sess = null;
	EIMUser user = null;

	// 更新時のユーザID・グループID・ロールID（オブジェクト取得(オブジェクトのnameと比較)）
	// 新規時はnull
	String objName = request.getParameter("objName");
	// ユーザ・グループ・ロールの切り分け情報
	//  ユーザ: user
	//  グループ: group
	//  ロール: role
	String objKey = request.getParameter("objKey");

	// メッセージ用
	String message = null;
	Object[] paramId = {
			"objName=" + objName,
			"objKey=" + objKey
			};

	try {
		// セッション取得
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessageValue(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessageValue("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		user = (EIMUser)sess.getAttribute("USER");

		// コンフィグのOBJECT_TYPE_NAME_***からオブジェクトタイプを判定
		// 流用したい場合は、else ifで増やすこと
		// ※また、セッションなどで直接オブジェクトIDを取得する方法もあります
		// 　そのときは、ソースを修正してください
		String configKey = null;
		if (objKey.equals("user")) {
			configKey = "OBJECT_TYPE_NAME_USER";
		} else if (objKey.equals("group")) {
			configKey = "OBJECT_TYPE_NAME_GROUP";
		} else if (objKey.equals("role")) {
			configKey = "OBJECT_TYPE_NAME_ROLE";
		} else {
			Object params1[] = {""};
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND.DETAIL", params1);
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessageValue("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND.DETAIL", params1);
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		String objTypeUser = EIMConfig.getValue(configKey);
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, objTypeUser);
		if (objType == null) {
			Object params2[] = {objTypeUser};
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND.DETAIL", params2);
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessageValue("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND.DETAIL", params2);
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		// オブジェクトタイプから属性を取得
		// List<EIMAttributeType> ...
		List attTypes = ObjectAttributeUtils.getAttributeTypeList(sess, objType);

		// ユーザID・グループID・ロールIDに関するオブジェクトを取得
		EIMObject object = null;
		if (objName != null) {
			object = ObjectUtils.getObjectByTypeAndName(sess, objType, objName);
			if (object == null) {
				Object params3[] = {"type=" + objType.getId() + ", name=" + objName};
				message = EIMResource.getMessageValue(sess, "EIM.ERROR.OBJECT.NOTFOUND.DETAIL", params3);
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessageValue("EIM.ERROR.OBJECT.NOTFOUND.DETAIL", params3);
				log.warn(AppMessageUtils.makeLogMessage(message));
				return;
			}
		}

		// 属性情報作成
		out.println("<attList>");

		for (int i = 0; i < attTypes.size(); i++) {
			EIMAttributeType attType = (EIMAttributeType)attTypes.get(i);
			EIMAttribute att = null;
			StringBuffer attValueListStr = new StringBuffer();		// 複数値用
			if (object != null) {
				att  = object.getAttribute(attType.getDefName());
			}
			String value = "";
			if (att != null) {
				switch (attType.getValueType().getId()) {
					// 数値型
					case EIMValueType.INTEGER:
						if (att.getInts() != null && att.getInts().length > 0) {
							value = String.valueOf(att.getInts()[0]);

							long[] valueInts = TypeConvertUtils.convertToLongArray(att.getInts());
							for (int j = 0; j < valueInts.length; j++) {
								attValueListStr.append(" <attMultiple attValue=\"" + valueInts[j] + "\"/>");
							}
						}
						
						break;
					// 文字列型
					case EIMValueType.STRING:
						if (att.getStrings() != null && att.getStrings().length > 0) {
							value = att.getStrings()[0];

							String[] valueStrings = att.getStrings();
							for (int j = 0; j < valueStrings.length; j++) {
								attValueListStr.append(" <attMultiple attValue=\"" + StringUtils.xmlEncode(valueStrings[j]) + "\"/>");
							}
						}
						break;
					// 日付型
					case EIMValueType.DATE:
						// タイムゾーン対応したlong型の値を取得   // convDBTzToCLTzExpirationTime   // getDBTzToCLTzDate   どっち
						if (att.getDates() != null && att.getDates().length > 0) {
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
						if (att.getTexts() != null && att.getTexts().length > 0) {
							value = att.getTexts()[0];

							String[] valueTexts = att.getTexts();
							for (int j = 0; j < valueTexts.length; j++) {
								attValueListStr.append(" <attMultiple attValue=\"" + StringUtils.xmlEncode(valueTexts[j]) + "\"/>");
							}
						}
						break;
						// ダブル型
					case EIMValueType.DOUBLE:
						if (att.getDoubles() != null && att.getDoubles().length > 0) {
							value = String.valueOf(att.getDoubles()[0]);

							double[] valueDoubles = att.getDoubles();
							for (int j = 0; j < valueDoubles.length; j++) {
								attValueListStr.append(" <attMultiple attValue=\"" + FormatUtil.getDoubleFormatedString(valueDoubles[j]) + "\"/>");
							}
						}

						break;
				}
			}
			if (attType.getValueType().getId() == EIMValueType.INTEGER ||
				attType.getValueType().getId() == EIMValueType.STRING ||
				attType.getValueType().getId() == EIMValueType.DATE ||
				attType.getValueType().getId() == EIMValueType.TEXT ||
				attType.getValueType().getId() == EIMValueType.DOUBLE ) {
				

				//複数値属性の入力欄が[追加]ボタンを押下してからでないと入力できないこと防止の為、空のカラム追加
				if (attValueListStr.length() == 0) {
					attValueListStr.append(" <attMultiple attValue=\"\"/>");
				}
				
				out.println("<attribute");
					out.println(" attTypeId=\"" + attType.getId() + "\"");
					out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
					out.println(" attTypeDefName=\"" + StringUtils.xmlEncode(attType.getDefName()) + "\"");
					out.println(" attValue=\"" + StringUtils.xmlEncode(value) + "\"");
					out.println(" valTypeId=\"" + attType.getValueType().getId() + "\"");
					out.println(" valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");
					out.println(" isMultiple=\"" + (attType.isMultiple()) + "\"");
				out.println(">");
				
				// 複数値用
				out.println("<attMultipleList>");
					out.println(attValueListStr.toString());
				out.println("</attMultipleList>");
				
				out.println("</attribute>");
			}
		}

		out.println("</attList>");
	} catch(EIMSysException eimse) {
		out.clear();
		message = EIMResource.getMessageValue(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(user.getId(), eimse.getMessage(), paramId), eimse);
	} catch(EIMAppException eimae) {
		out.clear();
		message = eimae.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eimae.getMessage(), paramId), eimae);
	} catch(EIMException eime) 	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
	} catch(Exception e) {
		out.clear();
		message = EIMResource.getMessageValue(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	} finally {
		try {
			if (sess != null) {
				sess.close();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>