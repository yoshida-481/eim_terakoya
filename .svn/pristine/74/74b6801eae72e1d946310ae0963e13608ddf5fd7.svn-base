package admin;

import java.util.TimeZone;

import org.xml.sax.SAXParseException;

import eimtest.app.util.net.EIMHttpServiceCaller;

/** */
public class AdminService extends EIMHttpServiceCaller {
	/** */
	boolean isGeneral = true;
	
	/**
	 * 
	 * @throws Exception
	 */
	public AdminService() throws Exception {
		this(false);
	}

	/**
	 * 
	 * @param otherLangNames
	 * @param appendParams
	 * @return o
	 */
	private Object[][] toOtherLangNameParam(String[][] otherLangNames, Object[][] appendParams)
	{
		Object[][] params = new Object[otherLangNames.length * 2 + 1 + appendParams.length][];
		int p = 0;
		params[p++] = new Object[] { "otherCnt", String.valueOf(otherLangNames.length) };
		for (int i = 0; i < otherLangNames.length; i++)
		{
			params[p++] = new Object[] { "otherLId" + i, otherLangNames[i][0] };
			params[p++] = new Object[] { "otherName" + i, otherLangNames[i][1] };
		}
		System.arraycopy(appendParams, 0, params, p, appendParams.length);
		return params;
	}

	/**
	 * 
	 * @param isGeneral
	 * @throws Exception
	 */
	public AdminService(boolean isGeneral) throws Exception {
		this(isGeneral, "system", "manager", null, null);
	}

	/**
	 * 
	 * @param isGeneral
	 * @param userCode
	 * @param userPass
	 * @param lang
	 * @param tz
	 * @throws Exception
	 */
	public AdminService(boolean isGeneral, String userCode, String userPass, String lang,
			TimeZone tz) throws Exception {
		super("admin");
		this.isGeneral = isGeneral;
		switchUser(userCode, userPass, lang, tz);
	}
	
	protected void preLogin() throws Exception {
		try
		{
			get("index.jsp" + (isGeneral ? "?appId=general" : ""), null);
		} catch (SAXParseException e)
		{
			// 非xhtmlのhtmlが返ってくるのでsaxs例外が発生する
		}
	}
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public String attribute_dspAttributeTypeList() throws Exception
	{
		return get("attribute/dspAttributeTypeList.jsp", new Object[][] { { "attTypeName", "*" } });
	}

	/**
	 * 
	 * @param objTypeId
	 * @return o
	 * @throws Exception
	 */
	public String object_dspAttributeType(int objTypeId) throws Exception
	{
		return get("object/dspAttributeType.jsp", new Object[][] { { "objTypeId",
				String.valueOf(objTypeId) } });
	}

	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_dspAttrTreeList() throws Exception
	{
		return get("attrTree/dspAttrTreeList.jsp", null);
	}

	/**
	 * 
	 * @param attrTreeItemId
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_dspAttrTreeItemList(int attrTreeItemId) throws Exception
	{
		return get("attrTree/dspAttrTreeItemList.jsp", new Object[][] { { "attrTreeId",
				String.valueOf(attrTreeItemId) } });
	}

	/**
	 * 
	 * @param attrTreeId
	 * @param attrTreeItemId
	 * @param viewNoValuesFlag
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_actUpdateAtrrTreeItemValue(int attrTreeId, int attrTreeItemId,
			boolean viewNoValuesFlag) throws Exception
	{
		return get("attrTree/actUpdateAtrrTreeItemValue.jsp", new Object[][] { //
				{ "attrTreeId", String.valueOf(attrTreeId) }//
						, { "attrTreeItemId", String.valueOf(attrTreeItemId) }//
						, { "viewNoValuesFlag", String.valueOf(viewNoValuesFlag) } //
				});
	}

	/**
	 * 
	 * @param attrTreeId
	 * @param attrTypeIdList
	 * @param viewNoValuesFlagList
	 * @param operationList
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_actUpdateAttrTreeItemList(int attrTreeId, String attrTypeIdList, 
			String viewNoValuesFlagList, String operationList)
			throws Exception
	{
		return get("attrTree/actUpdateAttrTreeItemList.jsp", new Object[][] { //
				{ "attrTreeId", String.valueOf(attrTreeId) }//
						, { "attrTypeIdList", attrTypeIdList } //
						, { "viewNoValuesFlagList", viewNoValuesFlagList } //
						, { "operationList", operationList } //
				});
	}

	/**
	 * 
	 * @param otherLangNames String[][]={{LangID,Name},{LangID,Name},...}
	 * @param classifyTarget
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_actCreateAttrTree(String[][] otherLangNames, int classifyTarget)
			throws Exception
	{
		return get("attrTree/actCreateAttrTree.jsp"//
				, toOtherLangNameParam(//
						otherLangNames//
						, new Object[][] { { "classifyTarget", String.valueOf(classifyTarget) } }//
				));
	}

	/**
	 * 
	 * @param attrTreeId
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_dspAttrTree(int attrTreeId) throws Exception
	{
		return get("attrTree/dspAttrTree.jsp", new Object[][] { //
				{ "attrTreeId", String.valueOf(attrTreeId) } //
				});
	}

	/**
	 * 
	 * @param attrTreeId
	 * @param otherLangNames String[][]={{LangID,Name},{LangID,Name},...}
	 * @param classifyTarget
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_actUpdateAttrTree(int attrTreeId, String[][] otherLangNames,
			int classifyTarget) throws Exception
	{
		return get("attrTree/actUpdateAttrTree.jsp"//
				, toOtherLangNameParam(//
						otherLangNames//
						, new Object[][] {//
						{ "attrTreeId", String.valueOf(attrTreeId) } //
								, { "classifyTarget", String.valueOf(classifyTarget) } //
						}));
	}

	/**
	 * 
	 * @param attrTreeId
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_actDeleteAttrTree(int attrTreeId) throws Exception
	{
		return get("attrTree/actDeleteAttrTree.jsp", new Object[][] { //
				{ "attrTreeId", String.valueOf(attrTreeId) } //
				});
	}

	/**
	 * 
	 * @param attrTypeName
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_dspAttributeTypeList(String attrTypeName) throws Exception
	{
		return get("attrTree/dspAttributeTypeList.jsp", new Object[][] { //
				{ "attTypeName", attrTypeName } //
				});
	}

	/**
	 * 
	 * @param attrTreeId
	 * @param attrTypeIds
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_actAddAttrTreeItem(int attrTreeId, String attrTypeIds) throws Exception
	{
		return get("attrTree/actAddAttrTreeItem.jsp", new Object[][] { //
				{ "attrTreeId", String.valueOf(attrTreeId) } //
						, { "attrTypeIds", attrTypeIds } //
				});
	}
}
