package common.util;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXParseException;

import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugIn.RecursiveTableEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;

/**
 *
 * カスタマイズデフォルトテーブル関連クラス
 *
 */
public class CustomDefaultTableUtils {

	/** Error Logging */
	private static Log log = LogFactory.getLog(CustomDefaultTableUtils.class);

	// 表示名称マップ(テーブル定義名称がキー、表示名称が値)
	private static Map<String, Map<String, String>> defNameLangNameMap = new HashMap<String, Map<String,String>>();

	private static final String XML_TAG_TABLELIST	= "tableList";
	private static final String XML_TAG_TABLEINFO	= "tableInfo";
	private static final String XML_TAG_NAME		= "name";
	private static final String XML_TAG_VALUE		= "value";
	private static final String XML_ATTR_DEFINITIONNAME	= "definitionName";
	private static final String XML_ATTR_LANG		= "lang";
	private static final String XML_ATTR_LANGNAME	= "langname";

	/**
	 * メニュー表示用XMLの文字列を作成する
	 * @param sess EIMSession
	 * @return メニュー表示用XML文字列
	 * @throws Exception
	 */
	public static String getListString(EIMSession sess) throws Exception {

		// メニュー表示用XML
		String listString = "";

		Document doc = null;
		try {
			doc = CustomDefaultTableConfig.getInstance().getDocument();
		} catch (SAXParseException se) {
			// カスタマイズデフォルトテーブル設定{0}が不正です。
			log.error(EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CUSTOMDEFAULT.XML", new Object[]{CustomDefaultTableConfig.getInstance().getFilePath()}));
			return listString;
		} catch (Exception e) {
			// システムエラーが発生しました。
			log.error(EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR"));
		}

		Element root = doc.getDocumentElement();

		// 言語ID
		String langId = sess.getLangId();
		// 選択しているカスタマイズデフォルトテーブルの定義名称
		String selectedTableDefName = getSelectedTableDefNameByUser(sess, sess.getUser());

		NodeList tableListNodeList = root.getElementsByTagName(XML_TAG_TABLELIST);

		List<String> stringList = new ArrayList<String>();

		try {
			// 表示名称のマップを取得する
			createLangNameMap(sess);

			if(tableListNodeList.item(0) != null){
				// 重複チェック用Set
				Set<String> tableDefNameDupCheckSet = new HashSet<String>();

				stringList.add("<customTableList>");
				// メニュー表示用XMLを作成
				createListStringRecursive(sess, langId, selectedTableDefName, stringList, (Element)tableListNodeList.item(0), tableDefNameDupCheckSet);
				stringList.add("</customTableList>");
			}

		} catch (EIMException e) {
			log.error(e.getMessage());
			stringList.clear();
			stringList.add("<customTableList>");
			stringList.add("</customTableList>");
		}


		for(String str : stringList){
			listString += str;
		}

		return listString;
	}

	/**
	 * メニュー表示用XMLの文字列を作成する
	 * @param sess EIMSession
	 * @param langId 言語ID
	 * @param selectedTableDefName 選択状態のカスタマイズデフォルトテーブル定義名称
	 * @param stringList メニュー表示用XMLの文字列
	 * @param tableElement 要素
	 * @param tableDefNameDupCheckSet カスタマイズデフォルトテーブル重複チェック用Set
	 * @throws Exception
	 */
	private static void createListStringRecursive(EIMSession sess, String langId, String selectedTableDefName, List<String> stringList, Element tableElement, Set<String> tableDefNameDupCheckSet) throws Exception {

		// 子供のノードを取得
		Node node = tableElement.getFirstChild();

		// 兄弟のノードを辿る
		while(node != null){
			if(node.getNodeType() != Node.ELEMENT_NODE){
				node = node.getNextSibling();
				continue;
			}
			Element element = (Element)node;
			String definitionName = element.getAttribute("definitionName");

			// 重複チェック
			if(tableDefNameDupCheckSet.contains(definitionName)){
				// カスタマイズデフォルトテーブル設定{0}でテーブル{1}が重複して設定されています。
				throw new EIMException("EIM.ERROR.LOGIC.CUSTOMDEFAULT.DUPLICATE.TABLE", new Object[]{CustomDefaultTableConfig.getInstance().getFilePath(), definitionName});
			}else{
				tableDefNameDupCheckSet.add(definitionName);
			}

			Map<String ,String> langMap = defNameLangNameMap.get(definitionName);
			// 表示名称が無い場合はメニューを作成しない
			if(langMap == null){
				// カスタマイズデフォルトテーブル設定{0}でテーブル{1}に表示名称が設定されていません。
				throw new EIMException("EIM.ERROR.LOGIC.CUSTOMDEFAULT.NO.NAME", new Object[]{CustomDefaultTableConfig.getInstance().getFilePath(), definitionName});
			}

			String label = langMap.get(langId);
			// 言語IDの表示名称が無い場合はメニューを作成しない
			if(label == null){
				// カスタマイズデフォルトテーブル設定{0}でテーブル{1}に表示名称が設定されていません。
				throw new EIMException("EIM.ERROR.LOGIC.CUSTOMDEFAULT.NO.NAME", new Object[]{CustomDefaultTableConfig.getInstance().getFilePath(), definitionName});
			}

			int selected = 0;
			if(hasChildElement(element)){
				stringList.add("<table label=\"" + label + "\" tableDefName=\"" + definitionName + "\" selected=\"" + selected + "\">");
				// 再帰で子階層を辿る
				createListStringRecursive(sess, langId, selectedTableDefName, stringList, element, tableDefNameDupCheckSet);
				stringList.add("</table>");
			}else{

				// 属性タイプのリストが無い場合(設定ファイルに定義されている属性タイプが１つでも存在しない場合)はメニューを作成しない
				List<EIMAttributeType> attributeTypeList = getAttributeTypeList(sess, definitionName);
				if(attributeTypeList.size() == 0){
					// カスタマイズデフォルトテーブル設定{0}でテーブル{1}に属性タイプが設定されていません。
					throw new EIMException("EIM.ERROR.LOGIC.CUSTOMDEFAULT.NO.ATTRIBUTETYPE", new Object[]{CustomDefaultTableConfig.getInstance().getFilePath(), definitionName});
				}
				if(definitionName.equals(selectedTableDefName)){
					selected = 1;
				}
				stringList.add("<table label=\"" + label + "\" tableDefName=\"" + definitionName + "\" selected=\"" + selected + "\" />");
			}

			node = node.getNextSibling();
		}
	}

	/**
	 * 子階層に要素が存在しているのかを確認する
	 * @return 要素が存在している場合はtrue、存在していない場合はfalse
	 * @param element
	 */
	private static boolean hasChildElement(Element element){
		boolean hasChildElement = false;

		// 子供のノードを取得
		Node node = element.getFirstChild();

		// 兄弟のノードを辿る
		while(node != null){
			if(node.getNodeType() == Node.ELEMENT_NODE){
				hasChildElement = true;
				break;
			}
			node = node.getNextSibling();
		}

		return hasChildElement;
	}

	/**
	 * 定義名称がキー、言語IDと言語名称が値のマップを作成する
	 * @param sess EIMSession
	 * @throws Exception
	 */
	private static void createLangNameMap(EIMSession sess) throws Exception {

		Document doc = CustomDefaultTableConfig.getInstance().getDocument();
		Element root = doc.getDocumentElement();

		// 重複チェック用Set テーブル定義
		Set<String> tableDefNameDupCheckSet = new HashSet<String>();
		// 重複チェック用Set 言語別表示名称
		Set<String> langIdDupCheckSet = new HashSet<String>();

		NodeList tableInfoNodeList = root.getElementsByTagName(XML_TAG_TABLEINFO);

		for(int i = 0; i < tableInfoNodeList.getLength(); i++){
			Node node = tableInfoNodeList.item(i);
			Element element = (Element)node;
			String definitionName = element.getAttribute(XML_ATTR_DEFINITIONNAME);

			if(tableDefNameDupCheckSet.contains(definitionName)){
				// カスタマイズデフォルトテーブル設定{0}でテーブル{1}が重複して設定されています。
				throw new EIMException("EIM.ERROR.LOGIC.CUSTOMDEFAULT.DUPLICATE.TABLE", new Object[]{CustomDefaultTableConfig.getInstance().getFilePath(), definitionName});
			}else{
				tableDefNameDupCheckSet.add(definitionName);
			}

			NodeList nameListNodeList = element.getElementsByTagName(XML_TAG_NAME);
			Map<String, String> langMap = new HashMap<String, String>();
			for(int j = 0; j < nameListNodeList.getLength(); j++){
				Element nameElement = (Element)nameListNodeList.item(j);
				String landId = nameElement.getAttribute(XML_ATTR_LANG);
				if(langIdDupCheckSet.contains(landId)){
					// カスタマイズデフォルトテーブル設定 {0} でテーブル {1} に表示名称 {2} が重複して設定されています。
					throw new EIMException("EIM.ERROR.LOGIC.CUSTOMDEFAULT.DUPLICATE.NAME", new Object[]{CustomDefaultTableConfig.getInstance().getFilePath(), definitionName, landId});
				}
				langIdDupCheckSet.add(landId);
				langMap.put(landId, nameElement.getAttribute(XML_ATTR_LANGNAME));
			}
			// 表示名称マップに追加
			defNameLangNameMap.put(definitionName, langMap);
			// 言語別表示名称の重複チェック用Setをクリア
			langIdDupCheckSet.clear();
		}

	}

	/**
	 * 引数のカスタマイズデフォルトテーブルの属性タイプのリストを取得する
	 * @param sess EIMSession
	 * @param tableDefName カスタマイズデフォルトテーブル定義名称
	 * @throws Exception
	 */
	private static List<EIMAttributeType> getAttributeTypeList(EIMSession sess, String tableDefName) throws Exception {

		// 返却用
		List<EIMAttributeType> attributeTypeList = new ArrayList<EIMAttributeType>();

		Document doc = CustomDefaultTableConfig.getInstance().getDocument();
		Element root = doc.getDocumentElement();
		NodeList tableInfoNodeList = root.getElementsByTagName(XML_TAG_TABLEINFO);

		// 重複チェック用Set
		Set<String> attrTypeDupCheckSet = new HashSet<String>();

		for(int i = 0; i < tableInfoNodeList.getLength(); i++){
			Node node = tableInfoNodeList.item(i);
			Element element = (Element)node;
			String definitionName = element.getAttribute(XML_ATTR_DEFINITIONNAME);
			if(!tableDefName.equals(definitionName)){
				continue;
			}

			NodeList attributeTypeListNodeList = element.getElementsByTagName(XML_TAG_VALUE);

			List<String> attrDefNameList = new ArrayList<String>();

			if(attributeTypeListNodeList.getLength() == 0){
				// 属性タイプが設定されていない場合
				break;
			}

			for(int j = 0; j < attributeTypeListNodeList.getLength(); j++){
				Element valueElement = (Element)attributeTypeListNodeList.item(j);
				String attributeTypeName = valueElement.getFirstChild().getNodeValue();
				attrDefNameList.add(attributeTypeName);

				if(attrTypeDupCheckSet.contains(attributeTypeName)){
					// カスタマイズデフォルトテーブル設定{0}でテーブル{1}に属性タイプ{2}が重複して設定されています。
					throw new EIMException("EIM.ERROR.LOGIC.CUSTOMDEFAULT.DUPLICATE.ATTRIBUTETYPE", new Object[]{CustomDefaultTableConfig.getInstance().getFilePath(), definitionName, attributeTypeName});

				}else{
					attrTypeDupCheckSet.add(attributeTypeName);
				}
			}

			// DBから属性タイプを取得
			Map<String, EIMAttributeType> attributeTypeMap = getAttributeTypeMap(sess, attrDefNameList);
			// DBからドキュメント・ワークスペース・フォルダ・タグに割り当てられている属性タイプを取得
			Map<String, EIMAttributeType> documentAttributeTypeMap = getDocumentAttributeTypeMap(sess, attrDefNameList);

			for(String attributeTypeDefName : attrDefNameList){
				EIMAttributeType attributeType = null;
				EIMAttributeType documentAttributeType = null;
				// マップから属性タイプを取得
				attributeType = attributeTypeMap.get(attributeTypeDefName);
				// ドキュメント管理のタイプのマップから属性タイプを取得
				documentAttributeType = documentAttributeTypeMap.get(attributeTypeDefName);
				if(attributeType != null && documentAttributeType != null){
					// 返却用リストに追加
					attributeTypeList.add(attributeType);
				}else if(attributeType == null){
					// カスタマイズデフォルトテーブル設定{0}で指定した属性タイプ{1}が存在しません。
					throw new EIMException("EIM.ERROR.LOGIC.CUSTOMDEFAULT.NOT.EXISTS.ATTRIBUTETYPE", new Object[]{CustomDefaultTableConfig.getInstance().getFilePath(), attributeTypeDefName});
				}else if(documentAttributeType == null){
					// カスタマイズデフォルトテーブル設定 {0} で指定した属性タイプ {1} はドキュメント管理で利用できる属性タイプではありません。
					throw new EIMException("EIM.ERROR.LOGIC.CUSTOMDEFAULT.NOT.USE.ATTRIBUTETYPE", new Object[]{CustomDefaultTableConfig.getInstance().getFilePath(), attributeTypeDefName});
				}

			}

		}

		return attributeTypeList;
	}

	/**
	 * 引数のカスタマイズデフォルトテーブルを選択状態にする
	 * 選択状態はEIMユーザオブジェクトの属性値に格納する
	 * @param sess EIMSession
	 * @param tableDefName カスタムデフォルトテーブル定義名称
	 * @throws Exception
	 */
	public static void updateSelected(EIMSession sess, String tableDefName) throws Exception {

		// ユーザーオブジェクトタイプ
		EIMObjectType userObjectType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_USER"));

		// 選択カスタマイズテーブル名称属性タイプ
		EIMAttributeType selectTableAttributeType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_USER_SELECT_CUSTOM_TABLE_NAME"));

		// セッションユーザーのオブジェクト
		EIMObject userObject = ObjectUtils.getObjectByTypeAndName(sess, userObjectType, Long.toString(sess.getUser().getId()));

		if(tableDefName != null){
			ObjectAttributeUtils.setAttribute(sess, userObject, selectTableAttributeType, tableDefName);
		}else{
			ObjectAttributeUtils.deleteAttribute(sess, userObject, selectTableAttributeType);
		}

		return;
	}

	/**
	 * ユーザーが選択しているカスタムデフォルトテーブルの属性タイプのリストを取得する
	 * @param sess EIMSession
	 * @param user EIMユーザ
	 * @return カスタムデフォルトテーブルの属性タイプのリスト
	 * @throws Exception
	 */
	public static List<EIMAttributeType> getSelectedTableAttributeTypeListByUser(EIMSession sess, EIMUser user) throws Exception {

		String selectedCustomDefaultTableName = getSelectedTableDefNameByUser(sess, user);

		if(selectedCustomDefaultTableName == null) return null;

		// マップから属性タイプリストを取得
		List<EIMAttributeType> attributeTypeList = getAttributeTypeList(sess, selectedCustomDefaultTableName);

		return attributeTypeList;
	}

	/**
	 * カスタムデフォルトテーブル定義名称の属性タイプのリストを取得する
	 * @param sess EIMSession
	 * @param definitionName カスタムデフォルトテーブル定義名称
	 * @return カスタムデフォルトテーブルの属性タイプのリスト
	 * @throws Exception
	 */
	public static List<EIMAttributeType> getSelectedTableAttributeTypeListByDefName(EIMSession sess, String definitionName) throws Exception {

		if(definitionName == null) return null;

		// マップから属性タイプリストを取得
		List<EIMAttributeType> attributeTypeList = getAttributeTypeList(sess, definitionName);

		return attributeTypeList;
	}

	/**
	 * ユーザーが選択しているカスタムデフォルトテーブルの定義名称を取得する
	 *
	 * @param sess EIMSession
	 * @param user EIMユーザ
	 * @return カスタムデフォルトテーブル定義名称
	 * @throws Exception
	 */
	public static String getSelectedTableDefNameByUser(EIMSession sess, EIMUser user) throws Exception {

		// ユーザーオブジェクトタイプ
		EIMObjectType userObjectType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_USER"));

		// セッションユーザーのオブジェクト
		EIMObject userObject = ObjectUtils.getObjectByTypeAndName(sess, userObjectType, Long.toString(sess.getUser().getId()));

		// 選択カスタマイズテーブル名称を取得
		EIMAttribute attribute = userObject.getAttribute(EIMConfig.get("ATTR_NAME_USER_SELECT_CUSTOM_TABLE_NAME"));

		if(attribute == null) return null;

		String selectedCustomDefaultTableName = attribute.getString();

		return selectedCustomDefaultTableName;
	}

	/**
	 * 引数の属性タイプ定義名称で属性タイプを取得する
	 * @param sess EIMSession
	 * @param attrDefNameList 属性タイプ定義名称のリスト
	 * @return 属性タイプ定義名称がキー、属性タイプが値のマップ
	 * @throws Exception
	 */
	private static Map<String, EIMAttributeType> getAttributeTypeMap(EIMSession sess, List<String> attrDefNameList) throws Exception {

		Map<String, EIMAttributeType> map = new HashMap<String, EIMAttributeType>();

		// SQL
		String sql = "";

		// Connection
		Connection conn = sess.getDBConnection();
		CallableStatement cstmt = null;
		ResultSet rset = null;

		try {

			StringBuffer inQuestions = new StringBuffer();

			for(int count = 0; count <  attrDefNameList.size(); count++){
				inQuestions.append(",?");
			}

			sql =  "select EA.id,EA.name,EAO.name as alias,EA.type,EA.is_multiple from eimattr EA, eimattrother EAO " +
			"where EA.name in (" + inQuestions.toString().replaceFirst(",","") + ") " +
			"and EA.id = EAO.aid and EAO.lid = ? ";

			// Prepare
			cstmt = conn.prepareCall(sql);

			int count = 0;
			// 定義名称
			for(count = 0; count <  attrDefNameList.size(); count++){
				cstmt.setString(count + 1, attrDefNameList.get(count));
			}

			// 言語ID
			cstmt.setString(++count, sess.getLangId());

			// フェッチサイズ
			cstmt.setFetchSize(10000);
			// Execute
			rset = cstmt.executeQuery();

			EIMAttributeType attributeType = null;
			while(rset.next()) {
				boolean isMultiple = false;
				if(rset.getInt("is_multiple") == 1){
					isMultiple = true;
				}
				String defName = rset.getString("name");
				attributeType = new EIMAttributeType(rset.getLong("id"),
						defName, rset.getString("alias"),
						EIMValueType.getTypeById(sess, rset.getInt("type")),
						isMultiple);
				map.put(defName, attributeType);
			}

		}catch(Exception e){
			throw e;
		}finally{
			if(rset != null){
				rset.close();
			}
			if(cstmt != null){
				cstmt.close();
			}
		}

		return map;

	}

	/**
	 * 引数の属性タイプ定義名称で属性タイプを取得する
	 * ただし、子タイプを含む「ドキュメント」・「ワークスペース」・「タグ」のオブジェクトタイプに
	 * 割り当たっている属性タイプのみが取得対象
	 * @param sess EIMSession
	 * @param attrDefNameList 属性タイプ定義名称のリスト
	 * @return 属性タイプ定義名称がキー、属性タイプが値のマップ
	 * @throws Exception
	 */
	private static Map<String, EIMAttributeType> getDocumentAttributeTypeMap(EIMSession sess, List<String> attrDefNameList) throws Exception {

		Map<String, EIMAttributeType> map = new HashMap<String, EIMAttributeType>();

		// SQL
		String sql = "";

		// Connection
		Connection conn = sess.getDBConnection();
		CallableStatement cstmt = null;
		ResultSet rset = null;

		try{

			StringBuffer inQuestions = new StringBuffer();

			for(int count = 0; count <  attrDefNameList.size(); count++){
				inQuestions.append(",?");
			}

			// オブジェクトタイプ再帰問合せパラメータ
			String[] columns = {"id"};
			String startCondition =String.format("name in (%s, %s, %s)",
					"'" + EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT") + "'" ,
					"'" + EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE") + "'" ,
					"'" + EIMConfig.get("OBJECT_TYPE_NAME_TAG") + "'");

			sql =  "select EA.id,EA.name,EAO.name as alias,EA.type,EA.is_multiple from eimattr EA, eimattrother EAO " +
			"where EA.name in (" + inQuestions.toString().replaceFirst(",","") + ") " +
			"and EA.id = EAO.aid and EAO.lid = ? " +
			"and exists( " +
			"    select 'X' from eimattrtype Y " +
			"    where EA.id = Y.id " +
			"    and Y.type in ( " +
					DatabasePlugInLoader.getPlugIn()
							.getQueryStringWithRecursive(RecursiveTableEnum.EIMOBJTYPE_CHILDREN, columns, startCondition, false) +
					DatabasePlugInLoader.getPlugIn()
							.getQueryStringSelectRecursive(RecursiveTableEnum.EIMOBJTYPE_CHILDREN, columns, startCondition, false) +
			"    ) " +
			") ";

			// Prepare
			cstmt = conn.prepareCall(sql);

			int count = 0;
			// 定義名称
			for(count = 0; count <  attrDefNameList.size(); count++){
				cstmt.setString(count + 1, attrDefNameList.get(count));
			}

			// 言語ID
			cstmt.setString(++count, sess.getLangId());

			// フェッチサイズ
			cstmt.setFetchSize(10000);
			// Execute
			rset = cstmt.executeQuery();

			EIMAttributeType attributeType = null;
			while(rset.next()) {
				boolean isMultiple = false;
				if(rset.getInt("is_multiple") == 1){
					isMultiple = true;
				}
				String defName = rset.getString("name");
				attributeType = new EIMAttributeType(rset.getLong("id"),
						defName, rset.getString("alias"),
						EIMValueType.getTypeById(sess, rset.getInt("type")),
						isMultiple);
				map.put(defName, attributeType);
			}

		}catch(Exception e){
			throw e;
		}finally{
			if(rset != null){
				rset.close();
			}
			if(cstmt != null){
				cstmt.close();
			}
		}

		return map;

	}

}
