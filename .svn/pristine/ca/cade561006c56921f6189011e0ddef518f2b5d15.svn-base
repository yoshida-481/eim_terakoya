package jp.co.ctc_g.eim.admin.business.service.impl;

import java.io.File;
import java.io.FileInputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.configuration2.HierarchicalConfiguration;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DataFormat;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import common.util.AdminAuthUtil;
import common.util.AppConstant;
import common.util.AppObjectUtil;
import common.util.CsvOutputUtil;
import common.util.EntryUtil;
import eim.bo.EIMException;
import eim.util.EIMXmlConfig;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.admin.business.dao.AdminUserDao;
import jp.co.ctc_g.eim.admin.business.domain.ExportDomain;
import jp.co.ctc_g.eim.admin.business.domain.criteria.AdminUserCriteria;
import jp.co.ctc_g.eim.admin.business.service.AdminUserService;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.GroupCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.RoleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.UserCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.GroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RoleDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.GroupService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.RoleService;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;

/**
 * AdminUserServiceの実装クラス
 *@see jp.co.ctc_g.eim.admin.business.service.AdminUserService
 */
public class AdminUserServiceImpl implements AdminUserService {

	/** 改行数*/
	private int addNewLine;

	/** 出力文字コード*/
	private String charset;

	/** 改行コード*/
	private String newline;

	/** ユーザサービス*/
	private UserService userService;

	/** グループサービス */
	private GroupService groupService;

	/** ロールサービス */
	private RoleService roleService;

	/** 属性タイプサービス */
	private AttributeTypeService attributeTypeService;

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService;

	/** システム管理のユーザ操作に関する機能DAO */
	private AdminUserDao adminUserDao;

	/** 言語日本語 */
	private final String LANG_JA = "JA";

	/** 言語英語 */
	private final String LANG_EN = "EN";

	/** ログ */
	private static Log log = LogFactory.getLog(AdminUserServiceImpl.class);

	/** 管理者権限に設定可能なビット長 */
	private final static int adminAuthBitLength = 32;

	/**
	 * ユーザ情報格納クラス
	 *
	 */
	private class UserVariables {

		/** ユーザドメイン */
		UserDomain tempUserDomain = new UserDomain();

		/** ユーザオブジェクト */
		ObjectDomain tempUserObjectDomain = new ObjectDomain();

		/** ユーザCode/拡張属性リストのマッピング */
		HashMap<String, List<AttributeDomain>> userIdAttributeDomainListMap = new HashMap<String, List<AttributeDomain>>();

		/** ユーザID */
		String userId;

		/** パスワード */
		String password;

		/** 新規登録ユーザドメイン/パスワードマッピング */
		HashMap<UserDomain, String> tempUserDomainPass = new HashMap<UserDomain, String>();

		/** ID/行番号マッピング */
		HashMap<String, Integer> inputId = new HashMap<String, Integer>();

		/** ID/ユーザドメインマッピング */
		HashMap<String, UserDomain> recordId = new HashMap<String, UserDomain>();

		/** Mail/行番号マッピング */
		HashMap<String, Integer> inputMail = new HashMap<String, Integer>();

		/** Mail/ユーザドメインマッピング */
		HashMap<String, UserDomain> recordMail = new HashMap<String, UserDomain>();

		/** 新規登録ユーザドメインリスト */
		ArrayList<UserDomain> newUserDomainList = new ArrayList<UserDomain>();

		/** 編集ユーザドメインリスト */
		ArrayList<UserDomain> editUserDomainList = new ArrayList<UserDomain>();

		/** 削除ユーザドメインリスト */
		ArrayList<UserDomain> deleteUserDomainList = new ArrayList<UserDomain>();

		/** 割当・割当解除グループマッピング */
		HashMap<String, List<List<GroupDomain>>> updateGroupMap = new HashMap<String, List<List<GroupDomain>>>();

		/** 割当・割当解除ロールマッピング */
		HashMap<String, List<List<RoleDomain>>> updateRoleMap = new HashMap<String, List<List<RoleDomain>>>();

		/** グループドメインのMap(キャッシュ用) */
		Map<String, GroupDomain> groupMap = new HashMap<String, GroupDomain>();

		/** グループドメインのMap(キャッシュ用) */
		Map<String, RoleDomain> roleMap = new HashMap<String, RoleDomain>();
	}


	/* (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.service.AdminUserService#checkExportInfo(java.lang.String, java.lang.String)
	 */
	public void checkExportInfo(ExportDomain exportDomain) throws Exception {

		// ログインユーザ情報取得
		UserDomain sessUser = EIMThreadContext.getTransactionContext().getUser();
		UserDomain loginUser = userService.getById(sessUser.getId());

		// 管理者権限チェック
		if (!AdminAuthUtil.hasAnyAuth(ConvertUtils.toEIMUser(loginUser))) {
			throw new EIMException("EIM.ERROR.LOGIC.NOADMINROLE");
		}

		// 文字コードチェック
		charset = CsvOutputUtil.getCharCodeSetting(null);

		// 改行コードチェック
		newline = ConfigUtils.getByKey("CSV_DOWNLOAD_NEWLINE");
		if(newline == null || newline.equals("")) {
			throw new EIMException("EIM.ERROR.LOGIC.CSVDOWNLOAD.NEWLINE.NOTFOUND");
		}

		//
		// ユーザ検索
		//

		// 検索条件の設定
		AdminUserCriteria adminUserCriteria = new AdminUserCriteria();

		if (exportDomain.getSearchUserCode() != null && exportDomain.getSearchUserCode().length() > 0)
			adminUserCriteria.setCode("*" + exportDomain.getSearchUserCode() + "*");

		if (exportDomain.getSearchUserName() != null && exportDomain.getSearchUserName().length() > 0)
			adminUserCriteria.setName("*" + exportDomain.getSearchUserName() + "*");

		if (exportDomain.getSearchUserMail() != null && exportDomain.getSearchUserMail().length() > 0)
			adminUserCriteria.setMail("*" + exportDomain.getSearchUserMail() + "*");

		if (Integer.toString(exportDomain.getIsNotDisplayInvalidityUser()).equals("1")) {
			// 有効ユーザのみ検索対象とする
			adminUserCriteria.setDisable(false);
		}

		else if (Integer.toString(exportDomain.getIsNotDisplayValidityUser()).equals("1")) {
			// 無効ユーザのみ検索対象とする
			adminUserCriteria.setDisable(true);
		}
		else {
			// 全ユーザを検索対象とする
			adminUserCriteria.setDisable(null);
		}

		if (exportDomain.getBelongingGroupName() != null && exportDomain.getBelongingGroupName().length() > 0) {
			adminUserCriteria.setGroupName("*" + exportDomain.getBelongingGroupName() + "*");
			adminUserCriteria.setIncludingChildGroup(String.valueOf(exportDomain.getIncludingChildGroup()).equals("true") ? true:false);
		}

		// 取得件数の制限
		adminUserCriteria.setLimit(Integer.parseInt(ConfigUtils.getByKey("GET_EXPORT_USERDATA_MAX_NUM")));
		adminUserCriteria.setLimitCondition(true);

		this.getList(adminUserCriteria);

	}


	/* (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.service.AdminUserService#exportUser(jp.co.ctc_g.eim.admin.business.domain.ExportDomain, jakarta.servlet.http.HttpServletResponse)
	 */
	@SuppressWarnings("unchecked")
	public Workbook exportUser(ExportDomain exportDomain) throws Exception {

		// 行番号
		int rowNum = 0;

		// ログインユーザ情報取得
		UserDomain loginUser = EIMThreadContext.getTransactionContext().getUser();

		// ログイン言語取得
		String langId = EIMThreadContext.getTransactionContext().getLangId();

		// ExportDomainデータ取得
		String searchUserCode = exportDomain.getSearchUserCode();
		String searchUserName = exportDomain.getSearchUserName();
		String appId = exportDomain.getAppName();
		String date = exportDomain.getDate();
		String fileName = exportDomain.getFileName();
		String hostName = exportDomain.getHostName();

		String searchUserMail = exportDomain.getSearchUserMail();
		String belongingGroupName = exportDomain.getBelongingGroupName();
		boolean includingChildGroup = exportDomain.getIncludingChildGroup();
		int isNotDisplayInvalidityUser = exportDomain.getIsNotDisplayInvalidityUser();
		int isNotDisplayValidityUser = exportDomain.getIsNotDisplayValidityUser();


		//
		// ユーザ検索
		//

		List<UserDomain> userList = new ArrayList<UserDomain>();

		// 検索条件の設定
		AdminUserCriteria adminUserCriteria = new AdminUserCriteria();

		if (exportDomain.getSearchUserCode() != null && exportDomain.getSearchUserCode().length() > 0)
			adminUserCriteria.setCode("*" + exportDomain.getSearchUserCode() + "*");

		if (exportDomain.getSearchUserName() != null && exportDomain.getSearchUserName().length() > 0)
			adminUserCriteria.setName("*" + exportDomain.getSearchUserName() + "*");

		if (exportDomain.getSearchUserMail() != null && exportDomain.getSearchUserMail().length() > 0)
			adminUserCriteria.setMail("*" + exportDomain.getSearchUserMail() + "*");

		if (Integer.toString(exportDomain.getIsNotDisplayInvalidityUser()).equals("1")) {
			// 有効ユーザのみ検索対象とする
			adminUserCriteria.setDisable(false);
		}

		else if (Integer.toString(exportDomain.getIsNotDisplayValidityUser()).equals("1")) {
			// 無効ユーザのみ検索対象とする
			adminUserCriteria.setDisable(true);
		}
		else {
			// 全ユーザを検索対象とする
			adminUserCriteria.setDisable(null);
		}

		if (exportDomain.getBelongingGroupName() != null && exportDomain.getBelongingGroupName().length() > 0) {
			adminUserCriteria.setGroupName("*" + exportDomain.getBelongingGroupName() + "*");
			adminUserCriteria.setIncludingChildGroup(String.valueOf(exportDomain.getIncludingChildGroup()).equals("true") ? true:false);
		}

		// 取得件数の制限
		adminUserCriteria.setLimit(Integer.parseInt(ConfigUtils.getByKey("GET_EXPORT_USERDATA_MAX_NUM")));
		adminUserCriteria.setLimitCondition(true);

		userList = this.getList(adminUserCriteria);


		// ユーザのリストをuserCodeでソートする
		List<UserDomain> sortedUserList = AppObjectUtil.getStrSortedList(userList, "getCode", true);

		// ワークブック取得(xlsx)
		XSSFWorkbook workbook = new XSSFWorkbook();

		// シート取得
		Sheet sheet = workbook.createSheet();

		// シート名設定(ファイル名から拡張子を除いた文字列を設定)
		workbook.setSheetName(0, fileName.replaceAll(ConfigUtils.getByKey("EXPORT_FILE_TYPE"), ""));

		// スタイルを生成(セル内改行)
		CellStyle newlineStyle = workbook.createCellStyle();
		// セル内改行をtrueにする
		newlineStyle.setWrapText(true);
		// 垂直中央揃え
		newlineStyle.setVerticalAlignment(VerticalAlignment.CENTER);

		// スタイルを生成(数値タイプ:文字列)
		DataFormat format = workbook.createDataFormat();
		CellStyle stringStyle = workbook.createCellStyle();
		stringStyle.setDataFormat(format.getFormat("text"));
		// 垂直中央揃え
		stringStyle.setVerticalAlignment(VerticalAlignment.CENTER);

		// 付加情報出力
		ArrayList<Row> row = new ArrayList<Row>();
		HashMap<String, Cell> cell = new HashMap<String, Cell>();

		for (int i = rowNum; i < 10; i++) {
			row.add(sheet.createRow(i));
			for (int j = 0; j < 2; j++) {
				cell.put(i + "," + j, row.get(i).createCell(j));
			}
			rowNum = i;
		}

		// 検索ユーザID
		cell.get("0,0").setCellValue(ResourceUtils.getByKey("LBL_SEARCH_USER_ID"));
		cell.get("0,1").setCellValue(searchUserCode);

		// 検索ユーザ名
		cell.get("1,0").setCellValue(ResourceUtils.getByKey("LBL_SEARCH_USER_NAME"));
		cell.get("1,1").setCellValue(searchUserName);

		// Mail
		cell.get("2,0").setCellValue(ResourceUtils.getByKey("LBL_USER_MAIL"));
		cell.get("2,1").setCellValue(searchUserMail);

		// グループ名
		cell.get("3,0").setCellValue(ResourceUtils.getByKey("LBL_GROUP_NAME"));
		cell.get("3,1").setCellValue(belongingGroupName);

		// 下位のグループを含む（チェックボックス）
		cell.get("4,0").setCellValue(ResourceUtils.getByKey("LBL_INCLUDING_CHILD_GROUP"));
		if(includingChildGroup) {
			cell.get("4,1").setCellValue(ResourceUtils.getByKey("LBL_ON"));
		} else {
			cell.get("4,1").setCellValue(ResourceUtils.getByKey("LBL_OFF"));
		}

		// 無効フラグ（ラジオボタン）
		cell.get("5,0").setCellValue(ResourceUtils.getByKey("LBL_INVALID_FLAG"));

		if(isNotDisplayInvalidityUser == 1 && isNotDisplayValidityUser == 0) {
			cell.get("5,1").setCellValue(ResourceUtils.getByKey("LBL_OFF"));
		} else if(isNotDisplayInvalidityUser == 0 && isNotDisplayValidityUser == 1) {
			cell.get("5,1").setCellValue(ResourceUtils.getByKey("LBL_ON"));
		} else {
			cell.get("5,1").setCellValue(ResourceUtils.getByKey("LBL_BOTH"));
		}

		// ログイン言語
		cell.get("6,0").setCellValue(ResourceUtils.getByKey("LBL_LOGIN_LANG"));
		cell.get("6,1").setCellValue(langId);

		// ログインユーザ名
		cell.get("7,0").setCellValue(ResourceUtils.getByKey("LBL_PRACTICE_USER_NAME"));
		cell.get("7,1").setCellValue(loginUser.getName());

		// エクスポート日時
		cell.get("8,0").setCellValue(ResourceUtils.getByKey("LBL_EXPORT_DATE"));
		cell.get("8,1").setCellValue(date);

		// 環境情報(サーバ名)
		cell.get("9,0").setCellValue(ResourceUtils.getByKey("LBL_SYSTEM_INFO"));
		cell.get("9,1").setCellValue(hostName);


		rowNum += 1;

		// 改行
		rowNum = newline(rowNum, addNewLine, sheet, row);

		// 管理者権限対応表出力
		EIMXmlConfig conf = EIMXmlConfig.getInstance();
		HierarchicalConfiguration hcAdminAuth = conf.getAdminAuthConfig();
		rowNum = writeAdminAuth(langId, appId, hcAdminAuth, rowNum, stringStyle, sheet, row, cell);

		// 改行
		rowNum = newline(rowNum, addNewLine, sheet, row);

		// ユーザ情報に設定されている拡張属性タイプのリストを取得
		ObjectTypeDomain tempObjectTypeDomain = objectTypeService.getByDefinitionName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_USER"));
		List<AttributeTypeDomain> userSetAttributeTypeList =tempObjectTypeDomain.getAttributeTypeList();

		// 取得した拡張属性タイプリストから、複数値、Object型、Code型、User型を除く
		List<AttributeTypeDomain> tempAttributeTypeList = new ArrayList<AttributeTypeDomain>();
		for(AttributeTypeDomain a : userSetAttributeTypeList) {

			// 拡張属性のタイプで処理分岐
			switch(a.getValueType()) {

			// 数値型の場合
			case LONG:
				if(!a.isMultiple()) {
					tempAttributeTypeList.add(a);
				}
				break;

			// DATE型の場合
			case DATE:
				if(!a.isMultiple()) {
					tempAttributeTypeList.add(a);
				}
				break;

			// DOUBLE型の場合
			case DOUBLE:
				if(!a.isMultiple()) {
					tempAttributeTypeList.add(a);
				}
				break;

			// STRING型の場合
			case STRING:
				if(!a.isMultiple()) {
					tempAttributeTypeList.add(a);
				}
				break;

			// TEXT型の場合
			case TEXT:
				if(!a.isMultiple()) {
					tempAttributeTypeList.add(a);
				}
				break;

			// CODE型の場合 (コード型のエクスポート/インポート対応)
			case CODE:
				if(!a.isMultiple()) {
					tempAttributeTypeList.add(a);
				}
				break;

			}
		}

		// 定義名称の降順でソート
		List<AttributeTypeDomain> sortedAttributeTypeList = AppObjectUtil.getStrSortedList(tempAttributeTypeList, "getDefinitionName", false);

		// ヘッダ行出力
		rowNum = makeHeader(rowNum, sheet, row, cell, sortedAttributeTypeList);

		// ユーザ情報出力
		writeUserData(sortedUserList, rowNum, sheet, row, cell, newlineStyle, stringStyle, sortedAttributeTypeList);

		// ワークブックを返却
		return workbook;
	}

	/**
	 * 	シートの改行を行います。
	 * @param rowNum 改行を行う行番号
	 * @param addNewLine 改行する行数
	 * @param sheet シート(poi)
	 * @param row 行(poi)
	 * @return 改行後の行番号
	 */
	private int newline(int rowNum, int addNewLine, Sheet sheet, ArrayList<Row> row) {
		for(int i = rowNum; i < rowNum + addNewLine; i++) {
			row.add(sheet.createRow(i));
		}
		return rowNum + addNewLine;
	}

	/**
	 * 	管理者権限対応表を出力します。
	 * @param langId ログイン言語ID
	 * @param appId アプリケーションID
	 * @param hcAdminAuth 設定ファイル情報
	 * @param rowNum 出力開始行番号
	 * @param stringStyle セルのスタイル(文字列)
	 * @param sheet シート(poi)
	 * @param row 行(poi)
	 * @param cell セル(poi)
	 * @return 管理者権限対応表出力後の行番号
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private int writeAdminAuth(String langId, String appId, HierarchicalConfiguration hcAdminAuth, int rowNum,CellStyle stringStyle, Sheet sheet, ArrayList<Row> row, HashMap<String, Cell> cell) throws Exception {
		row.add(sheet.createRow(rowNum));
		cell.put(rowNum + ",0", row.get(rowNum).createCell(0));
		cell.get(rowNum + ",0").setCellValue(ResourceUtils.getByKey("LBL_ADMIN_AUTH_TABLE"));
		rowNum += 1;

		int langNum;
		if(langId.equals(LANG_JA)) {
			langNum = 0;
		} else {
			langNum = 1;
		}
		int applicationMaxIndex = hcAdminAuth.getMaxIndex("authList.application");
		for(int i = 0; i <= applicationMaxIndex; i++) {
			String adminAppId = hcAdminAuth.getString("authList.application(" + i + ").[@id]");
			if(appId.equals(adminAppId)) {
				int authMaxIndex = hcAdminAuth.getMaxIndex("authList.application(" + i + ").auth");
				for(int j = 0; j <=authMaxIndex; j++) {
					String adminAuthName = hcAdminAuth.getString("authList.application(" + i + ").auth(" + j + ").nameList.name(" + langNum + ").[@langname]");
					String value = hcAdminAuth.getString("authList.application(" + i + ").auth(" + j + ").[@value]");
					String binary = Integer.toBinaryString(Integer.decode(value).intValue());
					StringBuilder sb = new StringBuilder();;
					if(binary.length() <= adminAuthBitLength) {
						for(int k = 0; k < adminAuthBitLength - binary.length(); k++) {
							sb.append("0");
						}
					}
					String binaryAdminAuth = sb.toString() + binary;
					row.add(sheet.createRow(rowNum));
					cell.put("" + rowNum +",0", row.get(rowNum).createCell(0));
					cell.get("" + rowNum +",0").setCellValue(adminAuthName);
					cell.put("" + rowNum +",1", row.get(rowNum).createCell(1));
					cell.get("" + rowNum +",1").setCellValue(binaryAdminAuth);
					cell.get("" + rowNum +",1").setCellStyle(stringStyle);
					rowNum += 1;
				}
				break;
			}
		}
		return rowNum;
	}

	/**
	 * 	ヘッダ行を出力します。
	 * @param rowNum 出力開始行番号
	 * @param sheet シート(poi)
	 * @param row 行(poi)
	 * @param cell セル(poi)
	 * @param sortedAttributeTypeList ユーザオブジェクトタイプに設定されている拡張属性タイプ
	 * @return ヘッダ行出力後の行番号
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private int makeHeader(int rowNum, Sheet sheet, ArrayList<Row> row, HashMap<String, Cell> cell, List<AttributeTypeDomain> sortedAttributeTypeList) throws Exception {
		row.add(sheet.createRow(rowNum));
		cell.put("" + rowNum +",0", row.get(rowNum).createCell(0));
		cell.get("" + rowNum +",0").setCellValue(ResourceUtils.getByKey("LBL_PROCESS_FLAG"));
		cell.put("" + rowNum +",1", row.get(rowNum).createCell(1));
		cell.get("" + rowNum +",1").setCellValue(ResourceUtils.getByKey("LBL_USER_ID"));
		cell.put("" + rowNum +",2", row.get(rowNum).createCell(2));
		cell.get("" + rowNum +",2").setCellValue(ResourceUtils.getByKey("LBL_USER_NAME_JA"));
		cell.put("" + rowNum +",3", row.get(rowNum).createCell(3));
		cell.get("" + rowNum +",3").setCellValue(ResourceUtils.getByKey("LBL_USER_NAME_EN"));
		cell.put("" + rowNum +",4", row.get(rowNum).createCell(4));
		cell.get("" + rowNum +",4").setCellValue(ResourceUtils.getByKey("LBL_USER_KANA"));
		cell.put("" + rowNum +",5", row.get(rowNum).createCell(5));
		cell.get("" + rowNum +",5").setCellValue(ResourceUtils.getByKey("LBL_USER_PASSWORD"));
		cell.put("" + rowNum +",6", row.get(rowNum).createCell(6));
		cell.get("" + rowNum +",6").setCellValue(ResourceUtils.getByKey("LBL_ADMIN_AUTH"));
		cell.put("" + rowNum +",7", row.get(rowNum).createCell(7));
		cell.get("" + rowNum +",7").setCellValue(ResourceUtils.getByKey("LBL_USER_MAIL"));
		cell.put("" + rowNum +",8", row.get(rowNum).createCell(8));
		cell.get("" + rowNum +",8").setCellValue(ResourceUtils.getByKey("LBL_INVALID_FLAG"));
		cell.put("" + rowNum +",9", row.get(rowNum).createCell(9));
		cell.get("" + rowNum +",9").setCellValue(ResourceUtils.getByKey("LBL_USER_MAIL_LANG"));
		cell.put("" + rowNum +",10", row.get(rowNum).createCell(10));
		cell.get("" + rowNum +",10").setCellValue(ResourceUtils.getByKey("LBL_GROUP"));
		cell.put("" + rowNum +",11", row.get(rowNum).createCell(11));
		cell.get("" + rowNum +",11").setCellValue(ResourceUtils.getByKey("LBL_ROLE"));

		// 拡張属性のヘッダを設定
		for(int i = 0; i < sortedAttributeTypeList.size(); i++) {
			cell.put("" + rowNum +","+ 12+i, row.get(rowNum).createCell(12+i));
			cell.get("" + rowNum +","+ 12+i).setCellValue(sortedAttributeTypeList.get(i).getDefinitionName());
		}

		return rowNum + 1;
	}

	/**
	 * 	ユーザ情報を出力します。
	 * @param sortedUserList 出力ユーザリスト
	 * @param rowNum 出力開始行番号
	 * @param sheet シート(poi)
	 * @param row 行(poi)
	 * @param cell セル(poi)
	 * @param newlineStyle セルのスタイル(セル内改行)
	 * @param stringStyle セルのスタイル(文字列)
	 * @param sortedAttributeTypeList 拡張属性タイプのリスト（複数値、Object型、Code型、User型を除き、定義名称降順でソート済み）
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	@SuppressWarnings("unchecked")
	private void writeUserData(List<UserDomain> sortedUserList, int rowNum, Sheet sheet, ArrayList<Row> row, HashMap<String, Cell> cell , CellStyle newlineStyle, CellStyle stringStyle,
			List<AttributeTypeDomain> sortedAttributeTypeList) throws Exception {
		int tempRowNum = rowNum;
		String tempWord;
		newline = ConfigUtils.getByKey("CSV_DOWNLOAD_NEWLINE");
		SimpleDateFormat sdf = new SimpleDateFormat(ResourceUtils.getByKey("EIM.FORMAT.DATETIME"));
		Map<Long, List<OtherNameDomain>> otherNameMap = new HashMap<>();
		if(sortedUserList.size() > 0) {
			otherNameMap = adminUserDao.getOtherNameListMapByIds(sortedUserList);
		}

		for(int i = 0; i < sortedUserList.size(); i++) {
			StringBuilder sbGroup = new StringBuilder();
			StringBuilder sbRole = new StringBuilder();
			UserDomain userdomain = sortedUserList.get(i);
			userdomain.setNameList(otherNameMap.get(userdomain.getId()));
			row.add(sheet.createRow(tempRowNum));
			cell.put("" + tempRowNum +",0", row.get(tempRowNum).createCell(0));
			cell.get("" + tempRowNum +",0").setCellValue("0");
			cell.put("" + tempRowNum +",1", row.get(tempRowNum).createCell(1));
			cell.get("" + tempRowNum +",1").setCellValue(userdomain.getCode());
			cell.get("" + tempRowNum +",1").setCellStyle(stringStyle);
			cell.put("" + tempRowNum +",2", row.get(tempRowNum).createCell(2));
			cell.get("" + tempRowNum +",2").setCellValue(userdomain.getDefinitionName());
			cell.put("" + tempRowNum +",3", row.get(tempRowNum).createCell(3));
			List<OtherNameDomain> nameList = userdomain.getNameList();
			cell.get("" + tempRowNum +",3").setCellValue(nameList.get(0).getName());
			cell.put("" + tempRowNum +",4", row.get(tempRowNum).createCell(4));
			if(userdomain.getKana() != null) {
				tempWord = userdomain.getKana();
			} else {
				tempWord = "";
			}
			cell.get("" + tempRowNum +",4").setCellValue(tempWord);
			cell.put("" + tempRowNum +",5", row.get(tempRowNum).createCell(5));
			cell.get("" + tempRowNum +",5").setCellValue("");
			cell.put("" + tempRowNum +",6", row.get(tempRowNum).createCell(6));
			String binary = Integer.toBinaryString(userdomain.getAdmin());
			StringBuilder sb = new StringBuilder();;
			if(binary.length() <= adminAuthBitLength) {
				for(int j = 0; j < adminAuthBitLength - binary.length(); j++) {
					sb.append("0");
				}
			}
			String binaryAdminAuth = sb.toString() + binary;
			cell.get("" + tempRowNum +",6").setCellValue(binaryAdminAuth);
			cell.get("" + tempRowNum +",6").setCellStyle(stringStyle);
			cell.put("" + tempRowNum +",7", row.get(tempRowNum).createCell(7));
			if(userdomain.getMail() != null) {
				tempWord = userdomain.getMail();
			} else {
				tempWord = "";
			}
			cell.get("" + tempRowNum +",7").setCellValue(tempWord);
			cell.put("" + tempRowNum +",8", row.get(tempRowNum).createCell(8));
			if(userdomain.isDisable() != true) {
				tempWord = "0";
			} else {
				tempWord = "1";
			}
			cell.get("" + tempRowNum +",8").setCellValue(tempWord);
			cell.put("" + tempRowNum +",9", row.get(tempRowNum).createCell(9));
			cell.get("" + tempRowNum +",9").setCellValue(userdomain.getLang());
			cell.put("" + tempRowNum +",10", row.get(tempRowNum).createCell(10));
			// グループのリストを取得
			List<GroupDomain> groupList = userdomain.getGroupList();
			// グループのリストをgroupNameでソートする
			List<GroupDomain> sortedGroupList = AppObjectUtil.getStrSortedList(groupList, "getName", true);
			for(int j = 0; j < sortedGroupList.size(); j++) {
				GroupDomain groupDomain = sortedGroupList.get(j);
				sbGroup.append(EntryUtil.getFullpathGroupName(groupDomain));
				if(j != sortedGroupList.size() - 1) {
					sbGroup.append(AppConstant.CSV_COLDELIMITER);
				} else {
					break;
				}
				sbGroup.append(newline);
			}
			cell.get("" + tempRowNum +",10").setCellValue(sbGroup.toString());
			cell.get("" + tempRowNum +",10").setCellStyle(newlineStyle);
			cell.put("" + tempRowNum +",11", row.get(tempRowNum).createCell(11));
			// ロールのリストを取得
			List<RoleDomain> roleList = userdomain.getRoleList();
			// ロールのリストをroleNameでソートする
			List<RoleDomain> sortedRoleList = AppObjectUtil.getStrSortedList(roleList, "getName", true);
			for(int j = 0; j < sortedRoleList.size(); j++) {
				RoleDomain roleDomain = sortedRoleList.get(j);
				sbRole.append(EntryUtil.getFullpathRoleName(roleDomain));
				if(j != sortedRoleList.size() - 1) {
					sbRole.append(AppConstant.CSV_COLDELIMITER);
				} else {
					break;
				}
				sbRole.append(newline);
			}
			cell.get("" + tempRowNum +",11").setCellValue(sbRole.toString());
			cell.get("" + tempRowNum +",11").setCellStyle(newlineStyle);

			// ユーザオブジェクトを取得
			ObjectDomain tempDomain = userdomain.getUserObject();

			if (tempDomain == null)
				continue;

			// ユーザオブジェクトに紐づく拡張属性をリストで取得
			List<AttributeDomain> attributeDomainList = tempDomain.getAttributeList();

			// 拡張属性値を出力
			for(int m = 0; m < sortedAttributeTypeList.size(); m++) {

				int extAttrRowNum = 12+m;

				for(AttributeDomain a : attributeDomainList) {
					if(sortedAttributeTypeList.get(m).getDefinitionName().equals(a.getAttributeType().getDefinitionName())) {

						cell.put("" + tempRowNum + "," + extAttrRowNum, row.get(tempRowNum).createCell(extAttrRowNum));

						// 拡張属性のタイプで処理分岐
						switch(a.getAttributeType().getValueType()) {

						// 数値型の場合
						case LONG:
							cell.get("" + tempRowNum + "," + extAttrRowNum).setCellStyle(stringStyle);
							Long extValueLong = a.getLong();
							if(extValueLong == null || extValueLong.equals(null)) {
								cell.get("" + tempRowNum + "," + extAttrRowNum).setCellValue("");
							} else {
								cell.get("" + tempRowNum + "," + extAttrRowNum).setCellValue(extValueLong);
							}
							break;

						// DATE型の場合
						case DATE:
							cell.get("" + tempRowNum + "," + extAttrRowNum).setCellStyle(stringStyle);
							Date extValueDate = a.getDate();
							if(extValueDate == null || extValueDate.equals(null)) {
								cell.get("" + tempRowNum +"," + extAttrRowNum).setCellValue("");
							} else {
								cell.get("" + tempRowNum +"," + extAttrRowNum).setCellValue(sdf.format(extValueDate));
							}
							break;

						// DOUBLE型の場合
						case DOUBLE:
							cell.get("" + tempRowNum + "," + extAttrRowNum).setCellStyle(stringStyle);
							Double extValueDouble = a.getDouble();
							if(extValueDouble == null || extValueDouble.equals(null)) {
								cell.get("" + tempRowNum +"," + extAttrRowNum).setCellValue("");
							} else {
								cell.get("" + tempRowNum +"," + extAttrRowNum).setCellValue(extValueDouble);
							}
							break;

						// STRING型の場合
						case STRING:
							cell.get("" + tempRowNum + "," + extAttrRowNum).setCellStyle(stringStyle);
							String extValueString = a.getString();
							if(extValueString == null || extValueString.equals(null)) {
								cell.get("" + tempRowNum +"," + extAttrRowNum).setCellValue("");
							} else {
								cell.get("" + tempRowNum +"," + extAttrRowNum).setCellValue(extValueString);
							}
							break;

						// TEXT型の場合
						case TEXT:
							cell.get("" + tempRowNum + "," + extAttrRowNum).setCellStyle(newlineStyle);
							String extValueText = a.getText();
							if(extValueText == null || extValueText.equals(null)) {
								cell.get("" + tempRowNum +"," + extAttrRowNum).setCellValue("");
							} else {
								cell.get("" + tempRowNum +"," + extAttrRowNum).setCellValue(extValueText);
							}
							break;

						// CODE型の場合 (コード型のエクスポート/インポート対応)
						case CODE:
							cell.get("" + tempRowNum + "," + extAttrRowNum).setCellStyle(newlineStyle);
							CodeDomain extValueCode = a.getCode();
							if(extValueCode == null || extValueCode.getCode() == null) {
								cell.get("" + tempRowNum +"," + extAttrRowNum).setCellValue("");
							} else {
								cell.get("" + tempRowNum +"," + extAttrRowNum).setCellValue(extValueCode.getCode());
							}
							break;

						}
					}
				}
			}
			tempRowNum += 1;
		}
	}

	/* (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.service.AdminUserService#importUser(java.lang.String)
	 */
	public String importUser(String filePath) throws Exception {

		String errorMessage = "";
		UserVariables userVariables = new UserVariables();

		try {
			// ログインユーザ情報取得
			UserDomain sessUser = EIMThreadContext.getTransactionContext().getUser();
			UserDomain loginUser = userService.getById(sessUser.getId());

			// 管理者権限チェック
			if (!AdminAuthUtil.hasAnyAuth(ConvertUtils.toEIMUser(loginUser))) {
				throw new EIMException("EIM.ERROR.LOGIC.NOADMINROLE");
			}

			// ワークブック読み込み(xlsx)
			FileInputStream fis = new FileInputStream(filePath);
			XSSFWorkbook workbook = new XSSFWorkbook(fis);

			// シート読み込み(index番号が0のシート)
			XSSFSheet sheet = workbook.getSheetAt(0);

			// ヘッダ行検索
			int startRowNum = searchStartRow(sheet);

			// ユーザ情報に設定されている拡張属性タイプをリストで取得
			ObjectTypeDomain tempObjectTypeDomain = objectTypeService.getByDefinitionName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_USER"));
			List<AttributeTypeDomain> userSetAttributeTypeList = tempObjectTypeDomain.getAttributeTypeList();

			// 取得した拡張属性タイプリストから、複数値、Object型、Code型、User型を除く
			List<AttributeTypeDomain> tempAttributeTypeList = new ArrayList<AttributeTypeDomain>();
			for(AttributeTypeDomain a : userSetAttributeTypeList) {

				// 拡張属性のタイプで処理分岐
				switch(a.getValueType()) {

				// 数値型の場合
				case LONG:
					if(!a.isMultiple()) {
						tempAttributeTypeList.add(a);
					}
					break;

				// DATE型の場合
				case DATE:
					if(!a.isMultiple()) {
						tempAttributeTypeList.add(a);
					}
					break;

				// DOUBLE型の場合
				case DOUBLE:
					if(!a.isMultiple()) {
						tempAttributeTypeList.add(a);
					}
					break;

				// STRING型の場合
				case STRING:
					if(!a.isMultiple()) {
						tempAttributeTypeList.add(a);
					}
					break;

				// TEXT型の場合
				case TEXT:
					if(!a.isMultiple()) {
						tempAttributeTypeList.add(a);
					}
					break;

				// CODE型の場合 (コード型のエクスポート/インポート対応)
				case CODE:
					if(!a.isMultiple()) {
						tempAttributeTypeList.add(a);
					}
					break;

				}
			}

			// 定義名称の降順でソート
			List<AttributeTypeDomain> sortedAttributeTypeList = AppObjectUtil.getStrSortedList(tempAttributeTypeList, "getDefinitionName", false);

			// ヘッダ行チェック
			errorMessage = headerCheck(startRowNum, sheet, sortedAttributeTypeList);

			// ヘッダ行にエラーがある場合、処理終了
			if(!StringUtils.isBlank(errorMessage)) {
				return errorMessage;
			}

			// インポート対象のヘッダを属性タイプリストに格納
			List<AttributeTypeDomain> importAttributeTypeList = new ArrayList<AttributeTypeDomain>();
			Row row = sheet.getRow(startRowNum);
			Cell cell = null;
			if(row != null) {
				// 「11」までは固定で基本属性が利用するため、「12」から処理を行う
				for(int i = 0; i <= sortedAttributeTypeList.size(); i++) {

					cell = row.getCell(12+i);

					// セルが空欄の場合、読み込み終了
					if(cell == null || StringUtils.isBlank(getCellData(cell))) {
						break;
					}

					// インポート対象の拡張属性タイプをimportAttributeTypeListに設定
					for(AttributeTypeDomain a : sortedAttributeTypeList) {
						if(a.getDefinitionName().equals(getCellData(cell))) {
							importAttributeTypeList.add(a);
							break;
						}
					}
				}
			}

			// 管理者権限対応表出力
			EIMXmlConfig conf = EIMXmlConfig.getInstance();
			HierarchicalConfiguration hcAdminAuth = conf.getAdminAuthConfig();

			// ユーザデータチェック
			errorMessage = userDataCheck(startRowNum, sheet, importAttributeTypeList, userVariables, hcAdminAuth);

			// ユーザデータにエラーがある場合、処理終了
			if(!StringUtils.isBlank(errorMessage)) {
				return errorMessage;
			}

			// インポート処理を実行(インポートの操作履歴出力も実行)
			executeImport(userVariables);

			return errorMessage;

		} finally {

			// 一時ファイル削除
			File file = new File(filePath);
			if (file.exists()) {
				if (file.delete()) {
					// EIM.INFO.LOGIC.DELETE.FILE.SUCCESS=ファイルを削除しました。
					log.info(ResourceUtils.getByKey("EIM.INFO.LOGIC.DELETE.FILE.SUCCESS"));
				} else {
					// EIM.ERROR.LOGIC.DELETE.FILE.FAIL=ファイルの削除に失敗しました。
					log.error(ResourceUtils.getByKey("EIM.ERROR.LOGIC.DELETE.FILE.FAIL"));
				}
			} else {
				// EIM.ERROR.LOGIC.FILE.NOT.FOUND=ファイルが見つかりません。
				log.error(ResourceUtils.getByKey("EIM.ERROR.LOGIC.FILE.NOT.FOUND"));
			}
		}
	}

	/**
	 * 読み込み始点ヘッダ「処理フラグ」検索
	 * @param sheet XSSFSheet
	 * @return 読み込み始点ヘッダ「処理フラグ」が存在する行番号
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private int searchStartRow(XSSFSheet sheet) throws Exception {
		int startRowNum = -1;
		Row row = null;
		Cell cell = null;

		// データが存在する最終行まで「処理フラグ」を検索
		for (int i = 0; i <= sheet.getLastRowNum(); i++) {
			row = sheet.getRow(i);
			if(row == null) {
				continue;
			}
			cell = row.getCell(0);
			if(cell == null) {
				continue;
			}
			if (getCellData(cell).equals(ResourceUtils.getByKey("LBL_PROCESS_FLAG"))) {
				startRowNum = i;
				break;
			}
		}
		return startRowNum;
	}

	/**
	 * ヘッダ行チェック
	 * @param startRowNum 読み込み始点ヘッダ「処理フラグ」が存在する行番号
	 * @param sheet XSSFSheet
	 * @param sortedAttributeTypeList ソート済みの拡張属性タイプリスト
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String headerCheck(int startRowNum, XSSFSheet sheet, List<AttributeTypeDomain> sortedAttributeTypeList) throws Exception {
		if(startRowNum == -1) {
			return ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.HEADER.EMPTY");
		}
		Row row = sheet.getRow(startRowNum);
		Cell cell = null;
		boolean isHeader = true;

		// 各ヘッダ名が正しいかチェック
		if(row != null) {
			for (int i = 1; i <= 12; i++) {
				if(isHeader == false) {
					return ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.HEADER.EMPTY");
				}
				cell = row.getCell(i);
				if(cell == null) {
					continue;
				}
				switch (i) {
				case 1:
					isHeader = getCellData(cell).equals(ResourceUtils.getByKey("LBL_USER_ID"));
					break;
				case 2:
					isHeader = getCellData(cell).equals(ResourceUtils.getByKey("LBL_USER_NAME_JA"));
					break;
				case 3:
					isHeader = getCellData(cell).equals(ResourceUtils.getByKey("LBL_USER_NAME_EN"));
					break;
				case 4:
					isHeader = getCellData(cell).equals(ResourceUtils.getByKey("LBL_USER_KANA"));
					break;
				case 5:
					isHeader = getCellData(cell).equals(ResourceUtils.getByKey("LBL_USER_PASSWORD"));
					break;
				case 6:
					isHeader = getCellData(cell).equals(ResourceUtils.getByKey("LBL_ADMIN_AUTH"));
					break;
				case 7:
					isHeader = getCellData(cell).equals(ResourceUtils.getByKey("LBL_USER_MAIL"));
					break;
				case 8:
					isHeader = getCellData(cell).equals(ResourceUtils.getByKey("LBL_INVALID_FLAG"));
					break;
				case 9:
					isHeader = getCellData(cell).equals(ResourceUtils.getByKey("LBL_USER_MAIL_LANG"));
					break;
				case 10:
					isHeader = getCellData(cell).equals(ResourceUtils.getByKey("LBL_GROUP"));
					break;
				case 11:
					isHeader = getCellData(cell).equals(ResourceUtils.getByKey("LBL_ROLE"));
					break;
				}
			}
		}


		boolean isExtHeader = false;
		List<String> extDefNameList = new ArrayList<String>();

		// 空白チェック（行の最終セルまで空白が存在しないこと）
		for(int c = 0; c < row.getLastCellNum() -12; c++) {
			cell = row.getCell(12+c);// 「12」までは基本属性が固定で利用
			// 定義名称を記載する部分が空白の場合、エラーとして処理終了
			if(cell == null || StringUtils.isBlank(getCellData(cell))) {
				return ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.HEADER.EMPTY");
			}
		}

		ObjectTypeDomain tempObjectTypeDomain = objectTypeService.getByDefinitionName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_USER"));
		List<AttributeTypeDomain> checkAttrTypeDomainList = tempObjectTypeDomain.getAttributeTypeList();

		// 基本属性部分（12項目）を除く拡張属性部分を対象としてエラーチェックと属性の取得を行う
		for(int k = 0; k < row.getLastCellNum() -12; k++){
			cell = row.getCell(12+k);// 「12」までは基本属性が固定で利用

			// 拡張属性の定義名称を取得
			AttributeTypeDomain checkAttrTypeDomain = null;
			for(AttributeTypeDomain a : checkAttrTypeDomainList) {
				if(a.getDefinitionName().equals(getCellData(cell))) {
					checkAttrTypeDomain = a;
					break;
				}
			}

			// 拡張属性の設定可否チェック
			if(checkAttrTypeDomain == null) {
				// 拡張属性({0})はユーザ情報に設定できません。
				return ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.ATTRTYPE.NOTREGISTERED", getCellData(cell));
			}

			// 複数値属性チェック
			if(checkAttrTypeDomain.isMultiple()) {
				// EIM.ERROR.LOGIC.IMPORT.HEADER.ISMULTIPLE=複数値属性 {0} はインポートできません。
				return ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.HEADER.ISMULTIPLE", getCellData(cell));
			}

			// Object型、Code型、User型の判定
			switch (checkAttrTypeDomain.getValueType()) {
			case OBJECT:
				// ({0})タイプの属性({1})はインポートできません。
				return ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.HEADER.UNIMPORTABLETYPE", checkAttrTypeDomain.getValueType().name(), getCellData(cell));

			case USER:
				// ({0})タイプの属性({1})はインポートできません。
				return ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.HEADER.UNIMPORTABLETYPE", checkAttrTypeDomain.getValueType().name(), getCellData(cell));

			}

			// エラー判定用変数
			isExtHeader = false;

			for(AttributeTypeDomain at : sortedAttributeTypeList) {
				if(getCellData(cell).equals(at.getDefinitionName())) {
					isExtHeader = true;
					// 同名の定義名称が2つヘッダに登録されている場合、エラーを返却
					for(String s : extDefNameList){
						if(s.equals(getCellData(cell))) {
							// 定義名称({0})が複数設定されています。
							return ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.HEADER.OVERLAP", s);
						}
					}
					extDefNameList.add(getCellData(cell));
					break;
				}
			}

			// ヘッダ名に合致する拡張属性タイプが登録されていない場合、エラーを返却
			if(!isExtHeader) {
				// 拡張属性({0})はユーザ情報に設定できません。
				return ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.ATTRTYPE.NOTREGISTERED", getCellData(cell));
			}
		}

		return "";
	}

	/**
	 *  ユーザデータチェック
	 * @param startRowNum 読み込み始点ヘッダ「処理フラグ」が存在する行番号
	 * @param sheet XSSFSheet
	 * @param importAttributeTypeList インポート対象となる拡張属性タイプのリスト
	 * @param userVariables ユーザ情報格納クラス
	 * @param hcAdminAuth 設定ファイル情報
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String userDataCheck(int startRowNum, XSSFSheet sheet,
			List<AttributeTypeDomain> importAttributeTypeList, UserVariables userVariables,
			HierarchicalConfiguration hcAdminAuth) throws Exception {
		StringBuilder errorMessage = new StringBuilder();
		int importUsers = 0;

		// 先にユーザ情報をまとめて取得
		List<String> userCodeList = new ArrayList<>();
		for(int i = startRowNum + 1; i <= sheet.getLastRowNum(); i++) {
			Row row = sheet.getRow(i);
			// 行がnullの場合、読み込み終了
			if(row == null) {
				break;
			}

			Cell targetCell = row.getCell(0);

			// セルが空欄の場合、読み込み終了
			if(targetCell == null || StringUtils.isBlank(getCellData(targetCell))) {
				break;
			}

			if(getCellData(targetCell).equals("0")) {
				// 処理対象外
				continue;
			}

			Cell userCodeCell = row.getCell(1);

			String code = getCellData(userCodeCell);
			if(!StringUtils.isBlank(code)) {
				userCodeList.add(code);
			}
		}

		Map<String, UserDomain> userMap = new HashMap<>();
		if(userCodeList.size() > 0) {
			userMap = adminUserDao.getListByCodes(userCodeList);
		}

		// 設定可能な管理者権限のビットをすべてONにした値を取得
		int sumOfAllAdminAuth = getSumOfAllAdminAuth(hcAdminAuth);

		// ヘッダ行以下のデータを読み込み
		for(int i = startRowNum + 1; i <= sheet.getLastRowNum(); i++) {
			Row row = sheet.getRow(i);
			// 行がnullの場合、読み込み終了
			if(row == null) {
				break;
			}
			Cell cell = row.getCell(0);
			// セルが空欄の場合、読み込み終了
			if(cell == null || StringUtils.isBlank(getCellData(cell))) {
				break;
			}

			// 処理フラグ入力値チェック
			if(getCellData(cell).equals("0") || getCellData(cell).equals("1") || getCellData(cell).equals("2") || getCellData(cell).equals("3")) {
				// 処理フラグの値で場合分け
				switch (Integer.parseInt(getCellData(cell))) {

				// 処理なし
				case 0:
					break;

				// ユーザ新規登録
				case 1:
					// ユーザIDチェック
					errorMessage.append(userIdCheck(row, cell, i, true, userVariables, userMap));
					// ユーザID以外のチェック
					errorMessage.append(userInfoCheck(row, cell, i, true, importAttributeTypeList, userVariables, sumOfAllAdminAuth));

					createUserDomainList(errorMessage.toString(), 1, userVariables);
					importUsers++;
					break;

				// ユーザ編集
				case 2:
					// ユーザIDチェック
					errorMessage.append(userIdCheck(row, cell, i, false, userVariables, userMap));
					// ユーザID以外のチェック
					errorMessage.append(userInfoCheck(row, cell, i, false, importAttributeTypeList, userVariables, sumOfAllAdminAuth));

					createUserDomainList(errorMessage.toString(), 2, userVariables);
					importUsers++;
					break;

				// ユーザ削除
				case 3:
					// ユーザIDチェック
					errorMessage.append(userIdCheck(row, cell, i, false, userVariables, userMap));
					// ユーザIDが存在する場合
					createUserDomainList(errorMessage.toString(), 3, userVariables);
					importUsers++;
					break;
				}

				// リセット
				userVariables.tempUserDomain = new UserDomain();
				userVariables.tempUserObjectDomain = new ObjectDomain();
				userVariables.password = null;

			} else {
				errorMessage.append(errorInfo(i, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.WRONG"), ResourceUtils.getByKey("LBL_PROCESS_FLAG")));
			}
		}

		// インポート上限チェック
		if(importUsers > Integer.parseInt(ConfigUtils.getByKey("GET_IMPORT_USERDATA_MAX_NUM"))) {
			errorMessage.append(ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.LIMIT.OVER", ConfigUtils.getByKey("GET_IMPORT_USERDATA_MAX_NUM")));
		}

		return errorMessage.toString();
	}

	/**
	 * IDチェック
	 * @param row 行
	 * @param cell セル
	 * @param rowNum 行番号
	 * @param isNewUser true:ユーザ新規登録、false:ユーザ編集orユーザ削除
	 * @param userVariables ユーザ情報格納クラス
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String userIdCheck(Row row, Cell cell,  int rowNum, boolean isNewUser, UserVariables userVariables, Map<String, UserDomain> userMap) throws Exception {
		StringBuilder errorMessage = new StringBuilder();
		cell = row.getCell(1);

		// セル入力チェック
		errorMessage.append(cellBlankCheck(cell, rowNum, ResourceUtils.getByKey("LBL_USER_ID")));
		if(!StringUtils.isBlank(errorMessage.toString())) {
			return errorMessage.toString();
		}

		userVariables.userId = getCellData(cell);
		// 文字数チェック
		if(!charactersCheck(userVariables.userId, true)) {
			errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.CHARACTERS"
					, ConfigUtils.getByKey("IMPORT_CHARACTER_MAX_NUM")), ResourceUtils.getByKey("LBL_USER_ID")));
		}

		// xlsxファイル内ID重複チェック
		// 重複が存在する場合
		if(userVariables.inputId.containsKey(userVariables.userId)) {
			int repeatedRowNum = userVariables.inputId.get(userVariables.userId) + 1;
			errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.REPEATED", repeatedRowNum), ResourceUtils.getByKey("LBL_USER_ID")));

		// 重複が存在しない場合
		} else {
			// IDと行番号をHashMapに格納
			userVariables.inputId.put(userVariables.userId, rowNum);
		}

		// 登録済みID重複チェック
		// 重複が存在する場合
		if(userVariables.recordId.containsKey(userVariables.userId)) {
			if(isNewUser == true) {
				errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.RECORD"), ResourceUtils.getByKey("LBL_USER_ID")));
				return errorMessage.toString();
			}

		// 重複が存在しない場合
		} else {
			UserDomain userDomain = userMap.get(userVariables.userId);
			if(userDomain != null) {
				userVariables.tempUserDomain = userDomain;

				userVariables.tempUserObjectDomain = userDomain.getUserObject();

				if(isNewUser == true) {
					errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.RECORD"), ResourceUtils.getByKey("LBL_USER_ID")));
				}
				// IDとユーザドメインをHashMapに格納
				userVariables.recordId.put(userVariables.userId, userDomain);
				// MailとユーザドメインをHashMapに格納
				String userMail = userDomain.getMail();
				userVariables.recordMail.put(userMail, userDomain);
			} else {
				if(isNewUser == false) {
					errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.USER"), ResourceUtils.getByKey("LBL_USER_ID")));
				}
			}
		}

		// ユーザドメインにID情報設定
		if(StringUtils.isBlank(errorMessage.toString())) {
			userVariables.tempUserDomain.setCode(userVariables.userId);
		}
		return errorMessage.toString();
	}

	/**
	 * ID以外のチェック
	 * @param row 行
	 * @param cell セル
	 * @param rowNum 行番号
	 * @param isNewUser true:ユーザ新規登録、false:ユーザ編集orユーザ削除
	 * @param importAttributeTypeList インポート対象となる拡張属性タイプのリスト
	 * @param userVariables ユーザ情報格納クラス
	 * @param sumOfAllAdminAuth 設定可能な管理者権限のすべてのビットをONにした値
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String userInfoCheck(Row row, Cell cell, int rowNum, boolean isNewUser,
			List<AttributeTypeDomain> importAttributeTypeList, UserVariables userVariables,
			int sumOfAllAdminAuth) throws Exception {
		StringBuilder errorMessage = new StringBuilder();

		// ユーザ名称チェック
		errorMessage.append(userNameCheck(row, cell, rowNum, userVariables));

		// かなチェック
		errorMessage.append(userKanaCheck(row, cell, rowNum, userVariables));

		// パスワードチェック
		errorMessage.append(userPassCheck(row, cell, rowNum, isNewUser, userVariables));

		// 管理者権限チェック
		errorMessage.append(userAdminAuthCheck(row, cell, rowNum, userVariables, sumOfAllAdminAuth));

		// Mailチェック
		errorMessage.append(userMailCheck(row, cell, rowNum, userVariables));

		// 無効フラグチェック
		errorMessage.append(userInvalidFlagCheck(row, cell, rowNum, userVariables));

		// 受信メール言語チェック
		errorMessage.append(userMailLangCheck(row, cell, rowNum, userVariables));

		// 拡張属性チェック
		errorMessage.append(userExtAttributeCheck(row, cell, rowNum, importAttributeTypeList, userVariables));


		List<GroupDomain> belongGroupList = new ArrayList<GroupDomain>();
		List<RoleDomain> belongRoleList = new ArrayList<RoleDomain>();

		UserDomain user = userVariables.recordId.get(userVariables.userId);
		if (user != null) {
			belongGroupList.addAll(user.getGroupList());
			belongRoleList.addAll(user.getRoleList());
		}

		// グループチェック
		cell = row.getCell(10);
		String group;
		if(cell == null) {
			group = "";
		} else {
			group = getCellData(cell);
		}

		List<List<GroupDomain>> updateGroupList = getUpdateGroupList(group, belongGroupList, userVariables);
		if (updateGroupList == null) {
			errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.GROUP"), ResourceUtils.getByKey("LBL_GROUP")));
		} else {
			userVariables.updateGroupMap.put(userVariables.userId, updateGroupList);
		}

		// ロールチェック
		cell = row.getCell(11);
		String role;
		if(cell == null) {
			role = "";
		} else {
			role = getCellData(cell);
		}

		List<List<RoleDomain>> updateRoleList = getUpdateRoleList(role, belongRoleList, userVariables);
		if (updateRoleList == null) {
			errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.ROLE"), ResourceUtils.getByKey("LBL_ROLE")));
		} else {
			userVariables.updateRoleMap.put(userVariables.userId, updateRoleList);
		}

		return errorMessage.toString();
	}

	/**
	 * ユーザ名称チェック
	 * @param row 行
	 * @param cell セル
	 * @param rowNum 行番号
	 * @param userVariables ユーザ情報格納クラス
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String userNameCheck(Row row, Cell cell, int rowNum, UserVariables userVariables) throws Exception {
		// ユーザ名(日本語)チェック
		StringBuilder errorMessage = new StringBuilder();
		cell = row.getCell(2);
		String userJaName = "";
		// セル入力チェック
		String errorMessageJa = cellBlankCheck(cell, rowNum, ResourceUtils.getByKey("LBL_USER_NAME_JA"));
		errorMessage.append(errorMessageJa);
		if(StringUtils.isBlank(errorMessageJa)) {
			// 文字数チェック
			userJaName = getCellData(cell);
			if(!charactersCheck(userJaName, true)) {
				errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.CHARACTERS"
						, ConfigUtils.getByKey("IMPORT_CHARACTER_MAX_NUM")), ResourceUtils.getByKey("LBL_USER_NAME_JA")));
			}
		}

		// ユーザ名(英語)チェック
		cell = row.getCell(3);
		String userEnName = "";
		// セル入力チェック
		String errorMessageEn = cellBlankCheck(cell, rowNum, ResourceUtils.getByKey("LBL_USER_NAME_EN"));
		errorMessage.append(errorMessageEn);
		if(StringUtils.isBlank(errorMessageEn)) {
			// 文字数チェック
			userEnName = getCellData(cell);
			if(!charactersCheck(userEnName, true)) {
				errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.CHARACTERS"
						, ConfigUtils.getByKey("IMPORT_CHARACTER_MAX_NUM")), ResourceUtils.getByKey("LBL_USER_NAME_EN")));
			}
		}

		// ユーザドメインにユーザ名称情報設定
		if(StringUtils.isBlank(errorMessage.toString())) {
			List <OtherNameDomain> nameList = createNameList(userJaName, userEnName);
			userVariables.tempUserDomain.setDefinitionName(userJaName);
			userVariables.tempUserDomain.setNameList(nameList);
		}

		return errorMessage.toString();
	}

	/**
	 * 名称リスト作成
	 * @param jaName 日本語名
	 * @param enName 英語名
	 * @return 名称リスト
	 */
	private List<OtherNameDomain> createNameList(String jaName , String enName) {
		List<OtherNameDomain> groupNameList = new ArrayList<OtherNameDomain>();
		OtherNameDomain jaGroupNameDomain = new OtherNameDomain();
		jaGroupNameDomain.setLangId(LANG_JA);
		jaGroupNameDomain.setName(jaName);
		groupNameList.add(jaGroupNameDomain);

		OtherNameDomain enGroupNameDomain = new OtherNameDomain();
		enGroupNameDomain.setLangId(LANG_EN);
		enGroupNameDomain.setName(enName);
		groupNameList.add(enGroupNameDomain);

		return groupNameList;
	}

	/**
	 * かなチェック
	 * @param row 行
	 * @param cell セル
	 * @param rowNum 行番号
	 * @param userVariables ユーザ情報格納クラス
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String userKanaCheck(Row row, Cell cell, int rowNum, UserVariables userVariables) throws Exception {
		cell = row.getCell(4);
		if(cell == null) {
			return "";
		}
		// 文字数チェック
		String userKana = getCellData(cell);
		if(!charactersCheck(userKana, false)) {
			return errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.CHARACTERS"
					, ConfigUtils.getByKey("IMPORT_CHARACTER_MAX_NUM")), ResourceUtils.getByKey("LBL_USER_KANA"));
		} else {
			// ユーザドメインにかな情報設定
			userVariables.tempUserDomain.setKana(userKana);
		}
		return "";
	}

	/**
	 * パスワードチェック
	 * @param row 行
	 * @param cell セル
	 * @param rowNum 行番号
	 * @param isNewUser isNewUser true:ユーザ新規登録、false:ユーザ編集orユーザ削除
	 * @param userVariables ユーザ情報格納クラス
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String userPassCheck(Row row, Cell cell, int rowNum, boolean isNewUser, UserVariables userVariables) throws Exception {
		cell = row.getCell(5);
		String userPass = "";
		String errorMessage = "";

		// ユーザ新規登録の場合
		if(isNewUser == true) {
			// セル入力チェック
			errorMessage = cellBlankCheck(cell, rowNum, ResourceUtils.getByKey("LBL_USER_PASSWORD"));
			if(StringUtils.isBlank(errorMessage)) {
				// 文字数チェック
				userPass = getCellData(cell);
				if(!charactersCheck(userPass, true)) {
					errorMessage = errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.CHARACTERS"
							, ConfigUtils.getByKey("IMPORT_CHARACTER_MAX_NUM")), ResourceUtils.getByKey("LBL_USER_PASSWORD"));
				}
				// 半角英数チェック
				if(!userPass.matches("[\\p{Punct}0-9a-zA-Z]+")) {
					errorMessage = errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.ALPHANUMERIC"), ResourceUtils.getByKey("LBL_USER_PASSWORD"));
				}
			}

		// ユーザ編集の場合
		} else {
			if(cell != null) {
				// 文字数チェック
				userPass = getCellData(cell);
				if(!charactersCheck(userPass, false)) {
					errorMessage = errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.CHARACTERS"
							, ConfigUtils.getByKey("IMPORT_CHARACTER_MAX_NUM")), ResourceUtils.getByKey("LBL_USER_PASSWORD"));
				}
				// 半角英数チェック
				if(!StringUtils.isBlank(userPass) && !userPass.matches("[\\p{Punct}0-9a-zA-Z]+")) {
					errorMessage = errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.ALPHANUMERIC"), ResourceUtils.getByKey("LBL_USER_PASSWORD"));
				}
			}
		}

		// ユーザドメインにパスワード情報設定
		if(StringUtils.isBlank(errorMessage)) {
			userVariables.password = userPass;
		}

		return errorMessage;
	}

	/**
	 * 管理者権限チェック
	 * @param row 行
	 * @param cell セル
	 * @param rowNum 行番号
	 * @param userVariables ユーザ情報格納クラス
	 * @param sumOfAllAdminAuth 設定可能な管理者権限のビットをすべてONにした値
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String userAdminAuthCheck(Row row, Cell cell, int rowNum,
			UserVariables userVariables, int sumOfAllAdminAuth) throws Exception {
		cell = row.getCell(6);
		// セル入力チェック
		String errorMessage = cellBlankCheck(cell, rowNum, ResourceUtils.getByKey("LBL_ADMIN_AUTH"));
		if(StringUtils.isBlank(errorMessage)) {
			// 2進数表記チェック
			String userAdminAuth = org.apache.commons.lang3.StringUtils.repeat("0", adminAuthBitLength) + getCellData(cell);
			userAdminAuth = userAdminAuth.substring(userAdminAuth.length() - adminAuthBitLength);
			boolean isCorrectBinary = true;
			for(int i = 0; i < adminAuthBitLength; i++) {

				if(!(userAdminAuth.charAt(i) == '0' || userAdminAuth.charAt(i) == '1')){
					isCorrectBinary = false;
					break;
				}

				if(userAdminAuth.charAt(i) == '1') {

					// 権限設定に関係しないインデックス番号
					int adminAuth = 1 << (adminAuthBitLength - i - 1);
					if((adminAuth & sumOfAllAdminAuth) == 0) {
						isCorrectBinary = false;
						break;
					}
				}

			}
			if(isCorrectBinary == false) {
				errorMessage = errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.WRONG"), ResourceUtils.getByKey("LBL_ADMIN_AUTH"));
			} else {
				// ユーザドメインに管理者権限情報設定
				int adminAuth = Integer.parseInt(userAdminAuth, 2);
				userVariables.tempUserDomain.setAdmin(adminAuth);
			}
		}
		return errorMessage;
	}

	/**
	 * 設定可能な管理者権限のビットをすべてONにした値を返却します。
	 *
	 * @param hcAdminAuth 設定ファイル情報
	 * @return 設定可能な管理者権限のビットをすべてONにした値
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private int getSumOfAllAdminAuth(HierarchicalConfiguration hcAdminAuth) throws Exception {

		int sumOfAllAdminAuth = 0;

		int applicationMaxIndex = hcAdminAuth.getMaxIndex("authList.application");
		for(int i = 0; i <= applicationMaxIndex; i++) {

			int authMaxIndex = hcAdminAuth.getMaxIndex("authList.application(" + i + ").auth");
			for(int j = 0; j <=authMaxIndex; j++) {
				String value = hcAdminAuth.getString("authList.application(" + i + ").auth(" + j + ").[@value]");
				sumOfAllAdminAuth += Integer.decode(value).intValue();
			}
		}

		return sumOfAllAdminAuth;

	}

	/**
	 * Mailチェック
	 * @param row 行
	 * @param cell セル
	 * @param rowNum 行番号
	 * @param userVariables ユーザ情報格納クラス
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String userMailCheck(Row row, Cell cell, int rowNum, UserVariables userVariables) throws Exception {
		StringBuilder errorMessage = new StringBuilder();

		cell = row.getCell(7);
		if(cell == null || StringUtils.isBlank(getCellData(cell))) {
			userVariables.tempUserDomain.setMail("");
			return "";
		}

		// 文字数チェック
		String userMail = getCellData(cell);
		if(!charactersCheck(userMail, false)) {
			errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.CHARACTERS"
					, ConfigUtils.getByKey("IMPORT_CHARACTER_MAX_NUM")), ResourceUtils.getByKey("LBL_USER_MAIL")));
		}

		// メールアドレスの重複が許可されていない場合、重複チェック
		/*
		"USER_MAILADDRESS_UNIQUE"はアプリ画面からユーザ新規登録orユーザ編集する際、メールアドレスの重複を許可するか否かの値(UserUtilsにて定義)
		true:重複不可能   false:重複可能

		AdminUserServiceImplでは、インポート時ユーザ新規登録orユーザ編集する場合に利用されるUserServiceImplのフィールドUserDao内で定義されている
		mailAddressUniqueの値もメールアドレスの重複を許可するか否かを判断しているが、この値が取得できないため、以下の条件式では"USER_MAILADDRESS_UNIQUE"
		のみを使用している
		*/
		if(ConfigUtils.getByKey("USER_MAILADDRESS_UNIQUE").equals("true")) {
			// xlsxファイル内ID重複チェック
			// 重複が存在する場合
			if(userVariables.inputMail.containsKey(userMail)) {
				int repeatedRowNum = userVariables.inputMail.get(userMail) + 1;
				errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.REPEATED", repeatedRowNum), ResourceUtils.getByKey("LBL_USER_MAIL")));

			// 重複が存在しない場合
			} else {
				// IDと行番号をHashMapに格納
				userVariables.inputMail.put(userMail, rowNum);
			}

			// 登録済みID重複チェック
			// 重複が存在する場合
			if(userVariables.recordMail.containsKey(userMail) && !userVariables.recordMail.get(userMail).getCode().equals(userVariables.userId)) {
				errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.RECORD"), ResourceUtils.getByKey("LBL_USER_MAIL")));
				return errorMessage.toString();

			// 重複が存在しない場合
			} else {
				// ユーザ検索
				UserCriteria userCriteria = new UserCriteria();
				userCriteria.setMail(userMail);
				List<UserDomain> userList = userService.getList(userCriteria);
				if(userList.size() != 0 && !userList.get(0).getCode().equals(userVariables.userId)) {
					errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.RECORD"), ResourceUtils.getByKey("LBL_USER_MAIL")));
					UserDomain userDomain = userList.get(0);
					// MailとユーザドメインをHashMapに格納
					userVariables.recordMail.put(userMail, userDomain);
					// IDとユーザドメインをHashMapに格納
					userVariables.recordId.put(userVariables.userId, userDomain);
				}
			}
		}

		// ユーザドメインにMail情報設定
		if(StringUtils.isBlank(errorMessage.toString())) {
			userVariables.tempUserDomain.setMail(userMail);
		}
		return errorMessage.toString();
	}

	/**
	 * 無効フラグチェック
	 * @param row 行
	 * @param cell セル
	 * @param rowNum 行番号
	 * @param userVariables ユーザ情報格納クラス
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String userInvalidFlagCheck(Row row, Cell cell, int rowNum, UserVariables userVariables) throws Exception {
		cell = row.getCell(8);
		// セル入力チェック
		String errorMessage = cellBlankCheck(cell, rowNum, ResourceUtils.getByKey("LBL_INVALID_FLAG"));
		if(StringUtils.isBlank(errorMessage)) {
			// 入力値チェック
			String userInvalidFlag = getCellData(cell);
			if(!(userInvalidFlag.equals("0") || userInvalidFlag.equals("1"))) {
				errorMessage = errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.WRONG"), ResourceUtils.getByKey("LBL_INVALID_FLAG"));
			} else {
				// ユーザドメインに無効フラグ情報設定
				if(userInvalidFlag.equals("0")) {
					userVariables.tempUserDomain.setDisable(false);
				} else {
					userVariables.tempUserDomain.setDisable(true);
				}
			}
		}
		return errorMessage;
	}

	/**
	 * 受信メール言語チェック
	 * @param row 行
	 * @param cell セル
	 * @param rowNum 行番号
	 * @param userVariables ユーザ情報格納クラス
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String userMailLangCheck(Row row, Cell cell, int rowNum, UserVariables userVariables) throws Exception {
		cell = row.getCell(9);
		// セル入力チェック
		String errorMessage = cellBlankCheck(cell, rowNum, ResourceUtils.getByKey("LBL_USER_MAIL_LANG"));
		if(StringUtils.isBlank(errorMessage)) {
			// 入力値チェック
			String userMailLang = getCellData(cell);
			if(!(userMailLang.equals(LANG_JA) || userMailLang.equals(LANG_EN))) {
				errorMessage = errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.WRONG"), ResourceUtils.getByKey("LBL_USER_MAIL_LANG"));
			} else {
				// ユーザドメインに受信メール言語情報設定
				userVariables.tempUserDomain.setLang(userMailLang);
			}
		}
		return errorMessage;
	}

	/**
	 * 拡張属性チェック
	 * @param row 行
	 * @param cell セル
	 * @param rowNum 行番号
	 * @param importAttributeTypeList インポート対象となる拡張属性タイプのリスト
	 * @param userVariables ユーザ情報格納クラス
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String userExtAttributeCheck(Row row, Cell cell, int rowNum, List<AttributeTypeDomain> importAttributeTypeList, UserVariables userVariables) throws Exception {

		StringBuilder errorMessage = new StringBuilder();
		AttributeDomain importAttribute = new AttributeDomain();
		List<AttributeDomain> importAttributeList = new ArrayList<AttributeDomain>();

		// 「11」までは固定で基本属性が利用するため、「12」から処理を行う
		for(int i = 0; i < importAttributeTypeList.size(); i++) {

			importAttribute = new AttributeDomain();
			cell = row.getCell(12+i);

			// 属性タイプ設定
			importAttribute.setAttributeType(importAttributeTypeList.get(i));

			// セルが空白ではない場合のみ、値をリストに格納
			if(cell == null || StringUtils.isBlank(getCellData(cell))) {
				// 処理を行わない

			} else {
				// 文字数チェック
				String extAttr = getCellData(cell);
				if(!charactersCheck(extAttr, false)) {
					errorMessage.append(errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.CHARACTERS"
							, ConfigUtils.getByKey("IMPORT_CHARACTER_MAX_NUM")), importAttribute.getAttributeType().getName()));
				} else {

					boolean bool = true;

					// 拡張属性のタイプで処理分岐
					switch(importAttribute.getAttributeType().getValueType()) {
					// 数値型の場合
					case LONG:
						long l = 0L;
						try {
							l = Long.parseLong(extAttr);
						} catch (NumberFormatException nfex) {
							bool = false;
							// rowNum行目：拡張属性({0})に設定された値をデータベースの型({1})に変換できません。
							errorMessage.append(errorInfo(
									rowNum,
									ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.ATTRTYPE.UNMATCH",
											importAttribute.getAttributeType().getName(), ResourceUtils.getByKey("EIM.VALUE.TYPE.NAME.INTEGER")),
									""));
						}

						if(bool){
							importAttribute.setLong(l);
							importAttributeList.add(importAttribute);
						}
						break;

					// DATE型の場合
					case DATE:
						SimpleDateFormat sdf = new SimpleDateFormat(ResourceUtils.getByKey("EIM.FORMAT.DATETIME"));
						sdf.setLenient(false);
						Date formatDate = new Date();

						try {
							formatDate = sdf.parse(extAttr);
						} catch (ParseException e) {
							bool = false;
							// rowNum行目：拡張属性({0})に設定された値をデータベースの型({1})に変換できません。
							errorMessage.append(errorInfo(
									rowNum,
									ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.ATTRTYPE.UNMATCH",
											importAttribute.getAttributeType().getName(), ResourceUtils.getByKey("EIM.VALUE.TYPE.NAME.DATE")),
									""));
						}

						if(bool) {
							importAttribute.setDate(formatDate);
							importAttributeList.add(importAttribute);
						}
						break;

					// DOUBLE型の場合
					case DOUBLE:
						Double d = 0.0;

						try {
							d = Double.parseDouble(extAttr);
						} catch (NumberFormatException nfex) {
							bool = false;
							// rowNum行目：拡張属性({0})に設定された値をデータベースの型({1})に変換できません。
							errorMessage.append(errorInfo(
									rowNum,
									ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.ATTRTYPE.UNMATCH",
											importAttribute.getAttributeType().getName(), ResourceUtils.getByKey("EIM.VALUE.TYPE.NAME.DOUBLE")),
									""));
						}

						if(bool){
							importAttribute.setDouble(d);
							importAttributeList.add(importAttribute);
						}
						break;

					// STRING型の場合
					case STRING:

						// STRING型として設定（型変換は行わない）
						importAttribute.setString(extAttr);
						importAttributeList.add(importAttribute);
						break;

					// TEXT型の場合
					case TEXT:

						// TEXT型として設定（型変換は行わない）
						importAttribute.setText(extAttr);
						importAttributeList.add(importAttribute);
						break;

					// CODE型の場合 (コード型のエクスポート/インポート対応)
					case CODE:
						CodeDomain newCode = null;
						CodeTypeDomain codeType = importAttribute.getAttributeType().getCodeType();

						bool = false;
						for (CodeDomain code : codeType.getCodeList()) {
							if (code.getCode().equals(extAttr)) {
								bool = true;
								newCode = code;
								break;
							}
						}

						if(bool){
							importAttribute.setCode(newCode);
							importAttributeList.add(importAttribute);
						}
						else {
							// rowNum行目：拡張属性({0})に設定された値をデータベースの型({1})に変換できません。
							errorMessage.append(errorInfo(
									rowNum,
									ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.ATTRTYPE.UNMATCH",
											importAttribute.getAttributeType().getName(), ResourceUtils.getByKey("EIM.VALUE.TYPE.NAME.DOUBLE")),
									""));

						}
						break;

					}
				}
			}
		}

		// インポート対象の拡張属性リストをドメインに設定
		userVariables.tempUserObjectDomain.setAttributeList(importAttributeList);

		// ユーザIDを取得し、拡張属性リストとマッピング
		cell = row.getCell(1);
		userVariables.userIdAttributeDomainListMap.put(getCellData(cell), importAttributeList);

		return errorMessage.toString();
	}

	/**
	 * ユーザドメインリスト作成
	 * @param errorMessage エラーメッセージ
	 * @param processFlag 処理フラグ
	 * @param userVariables ユーザ情報格納クラス
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private void createUserDomainList(String errorMessage, int processFlag, UserVariables userVariables) throws Exception {

		// チェック時にエラーがない場合のみ、ユーザドメインリスト作成
		if(StringUtils.isBlank(errorMessage)) {
			switch(processFlag) {

			// ユーザ新規登録
			case 1:
				userVariables.newUserDomainList.add(userVariables.tempUserDomain);
				userVariables.tempUserDomainPass.put(userVariables.tempUserDomain, userVariables.password);
				break;

			// ユーザ編集
			case 2:
				userVariables.editUserDomainList.add(userVariables.tempUserDomain);
				userVariables.tempUserDomainPass.put(userVariables.tempUserDomain, userVariables.password);
				break;

			// ユーザ削除
			case 3:
				userVariables.deleteUserDomainList.add(userVariables.tempUserDomain);
				break;
			}
		}
	}

	/**
	 * インポート処理を実行
	 * ユーザドメインのリストごとにユーザ新規登録/ユーザ編集/ユーザ削除を実行
	 * ユーザ新規登録ではグループ・ロールの割当も実行
	 * ユーザ編集ではグループ・ロールの割当/割当解除も実行
	 * @param userVariables ユーザ情報格納クラス
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private void executeImport(UserVariables userVariables) throws Exception {
		UserDomain userDomain;

		// ユーザ新規登録
		for(int i = 0; i < userVariables.newUserDomainList.size(); i++) {
			userDomain = userVariables.newUserDomainList.get(i);
			String pass = userVariables.tempUserDomainPass.get(userDomain);

			ObjectDomain updateTargetDomain = new ObjectDomain();
			List<AttributeDomain> userExtAttrList = userVariables.userIdAttributeDomainListMap.get(userDomain.getCode());
			updateTargetDomain.setAttributeList(userExtAttrList);
			userDomain.setUserObject(updateTargetDomain);
			userService.create(userDomain, pass);

			// グループ割当
			updateBelongGroup(userDomain, userVariables.updateGroupMap.get(userDomain.getCode()));
			// ロール割当
			updateBelongRole(userDomain, userVariables.updateRoleMap.get(userDomain.getCode()));
		}

		// ユーザ編集
		for(int i = 0; i < userVariables.editUserDomainList.size(); i++) {
			userDomain = userVariables.editUserDomainList.get(i);
			String pass = userVariables.tempUserDomainPass.get(userDomain);

			ObjectDomain updateTargetDomain = new ObjectDomain();
			List<AttributeDomain> userExtAttrList = userVariables.userIdAttributeDomainListMap.get(userDomain.getCode());
			updateTargetDomain.setAttributeList(userExtAttrList);
			userDomain.setUserObject(updateTargetDomain);
			userService.update(userDomain);

			// パスワード更新
			if(!StringUtils.isBlank(pass)) {
				userService.updatePassword(userDomain, pass);
			}

			// グループ割当・割当解除
			updateBelongGroup(userDomain, userVariables.updateGroupMap.get(userDomain.getCode()));
			// ロール割当・割当解除
			updateBelongRole(userDomain, userVariables.updateRoleMap.get(userDomain.getCode()));

		}

		// ユーザ削除
		for(int i = 0; i < userVariables.deleteUserDomainList.size(); i++) {
			userDomain = userVariables.deleteUserDomainList.get(i);

			userService.delete(userDomain);
		}
	}

	/**
	 * 行番号&エラーメッセージ出力
	 * @param rowNum 行番号
	 * @param info エラー内容
	 * @param ColumnName カラム名
	 * @return エラー詳細
	 */
	private String errorInfo(int rowNum, String info, String ColumnName) {
		if(StringUtils.isBlank(ColumnName)) {
			return String.valueOf(rowNum + 1) + "行目：" + info + "\r";
		} else {
			return String.valueOf(rowNum + 1) + "行目：" + "[" + ColumnName + "]" + info + "\r";
		}
	}

	/**
	 * セル入力チェック
	 * @param cell セル
	 * @param rowNum 行番号
	 * @param columnName カラム名
	 * @return エラー詳細
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private String cellBlankCheck(Cell cell, int rowNum, String columnName) throws Exception {
		if(cell == null || StringUtils.isBlank(getCellData(cell))) {
			String errorMessage = (errorInfo(rowNum, ResourceUtils.getByKey("EIM.ERROR.LOGIC.IMPORT.BLANK"), columnName));
			return errorMessage;
		}
		return "";
	}

	/**セル内容取得
	 * @param cell セル
	 * @return セル入力値(String)
	 */
	private String getCellData(Cell cell) throws Exception {
		String cellData = "";
		// セルタイプ取得
		CellType cellType = cell.getCellType();
		switch (cellType) {
		case NUMERIC:
			if(DateUtil.isCellDateFormatted(cell)) {
				// 日付タイプの場合、文字列型に変換して返却
				cellData = new SimpleDateFormat(ResourceUtils.getByKey("EIM.FORMAT.DATETIME")).format(cell.getDateCellValue());

			} else {
				// 数値タイプの場合（int or double を判定しString型で返却）
				int ii = (int)cell.getNumericCellValue();
				double dd = cell.getNumericCellValue();

				if(ii == dd) {
					// int型からString型に変換して返却
					cellData = String.valueOf(ii);
				} else {
					// double型からString型に変換して返却
					cellData = String.valueOf(dd);
				}
			}
			break;
		case STRING:
			cellData = cell.getStringCellValue();
			break;
		case FORMULA:
			cellData = cell.getCellFormula();
			break;
		case BLANK:
			cellData = cell.getStringCellValue();
			break;
		case BOOLEAN:
			boolean booleanCell = cell.getBooleanCellValue();
			cellData = String.valueOf(booleanCell);
			break;
		case ERROR:
			byte errorCell = cell.getErrorCellValue();
			cellData = String.valueOf(errorCell);
			break;
		default:
			break;
		}
		return cellData;
	}

	/**
	 * 入力文字数チェック
	 * @param word 文字列
	 * @param isNecessary true:入力必須項目、false:非入力必須項目
	 * @return
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	private boolean charactersCheck(String word, boolean isNecessary) throws Exception {
		// 入力可能最大文字数
		int limitChar = Integer.valueOf(ConfigUtils.getByKey("IMPORT_CHARACTER_MAX_NUM"));

		// 入力必須項目
		if(isNecessary == true) {
			// 1字以上127字以下であることを判定
			if(1 <= word.length() && word.length() <= limitChar) {
				return true;
			} else {
				return false;
			}

		// 非入力必須項目
		} else {
			// 0字以上127字以下であることを判定
			if(0 <= word.length() && word.length() <= limitChar) {
				return true;
			} else {
				return false;
			}
		}
	}

	/**
	 * グループ存在チェック,割当・割当解除リスト作成の処理。<br>
	 * <br>
	 * フルパスを取得<br>
	 * カンマ区切りで分割。<br>
	 * パスの存在チェック<br>
	 * pathStrに存在しないグループのパスが含まれている場合、nullを返却します。<br>
	 *
	 * @param pathStr カンマ区切りのフルパス
	 * @param belongGroupList ユーザが現在所属しているグループリスト（新規は空）
	 * @return グループ割当・割当解除リストのリスト(0:割当グループリスト,1:割当解除グループリスト)
	 * @throws Exception
	 */
	private List<List<GroupDomain>> getUpdateGroupList(String pathStr, List<GroupDomain> belongGroupList, UserVariables userVariables) throws Exception {

		// 返却値
		List<List<GroupDomain>> returnList = new ArrayList<List<GroupDomain>>();

		List<GroupDomain> registGroupList = new ArrayList<GroupDomain>();
		List<GroupDomain> deleteGroupList = new ArrayList<GroupDomain>();

		returnList.add(registGroupList);
		returnList.add(deleteGroupList);

		// 改行コードの除去
		pathStr = pathStr.replaceAll("\r", "");
		pathStr = pathStr.replaceAll("\n", "");

		// フルパスをカンマ区切りで分割する。
		String[] pathList = pathStr.split(",");
		ArrayList<String> array = new ArrayList<String>();
		for (String path : pathList) {
			if(!array.contains(path)) {
				array.add(path);
			}
		}
		// 以下の処理はパスの数だけ繰り返す
		for (String path : array) {

			// カンマ連続時のスキップ処理
			if (path.length() == 0) {
				continue;
			}

			GroupDomain targetGroup = null;

			// キャッシュ内存在チェック
			if (userVariables.groupMap.get(path) != null) {

			targetGroup = userVariables.groupMap.get(path);

			} else {
				// パス分割
				String[] pathArray = path.split("/");
				if (pathArray.length != 0) {

					// DB存在チェック
					GroupCriteria groupCriteria = new GroupCriteria();
					groupCriteria.setName(pathArray[pathArray.length - 1]);
					// パスからグループ取得の処理
					for (GroupDomain groupDomain : groupService.getList(groupCriteria)) {
						if (EntryUtil.getFullpathGroupName(groupDomain).equals(path)) {
							targetGroup = groupDomain;
							// キャッシュに保持
							userVariables.groupMap.put(path, groupDomain);
							break;
						}
					}
					if (targetGroup == null) {
						// グループがDBに存在しない場合null返却
						return null;
					}
				} else {
					// パスが不正
					return null;
				}
			}
			// 割当判定
			boolean isRegist = true;
			GroupDomain belongGroup = new GroupDomain();
			for (GroupDomain group : belongGroupList) {
				if (group.getId() == targetGroup.getId()) {
					isRegist = false;
					belongGroup = group;
				}
			}
			if (isRegist) {
				// 割当対象
				registGroupList.add(targetGroup);
			} else {
				// 更新不要
				belongGroupList.remove(belongGroup);
			}
		}
		// 割当解除対象
		for (GroupDomain group : belongGroupList) {
			deleteGroupList.add(group);
		}
		return returnList;
	}

	/**
	 * グループ割当・削除の処理。<br>
	 * <br>
	 *
	 * @param user ユーザ
	 * @param groupList グループ割当・削除処理済List(getGroupMapメソッドのreturn値)
	 * @throws Exception
	 */
	private void updateBelongGroup(UserDomain user, List<List<GroupDomain>> groupList) throws Exception {
		if (groupList == null) {
			return;
		}
		// 割当
		for (GroupDomain registGroup : groupList.get(0)) {
			groupService.addUser(registGroup, user);
		}
		// 割当解除
		for (GroupDomain deleteGroup : groupList.get(1)) {
			groupService.removeUser(deleteGroup, user);
		}
	}

	/**
	 * ロール存在チェック,割当・割当解除リスト作成の処理。<br>
	 * <br>
	 * フルパスを取得<br>
	 * カンマ区切りで分割。<br>
	 * パスの存在チェック<br>
	 * pathStrに存在しないロールのパスが存在する場合、nullを返却します。<br>
	 * <br>
	 *
	 * @param pathStr カンマ区切りのフルパス
	 * @param belongRoleList ユーザが現在所属しているロールリスト
	 * @return ロール割当・割当解除リストのリスト(0:割当ロールリスト,1:割当解除ロールリスト)
	 * @throws Exception
	 */
	private List<List<RoleDomain>> getUpdateRoleList(String pathStr, List<RoleDomain> belongRoleList, UserVariables userVariables) throws Exception {

		// 返却値
		List<List<RoleDomain>> returnList = new ArrayList<List<RoleDomain>>();

		List<RoleDomain> registRoleList = new ArrayList<RoleDomain>();
		List<RoleDomain> deleteRoleList = new ArrayList<RoleDomain>();

		returnList.add(registRoleList);
		returnList.add(deleteRoleList);

		// 改行コードの除去
		pathStr = pathStr.replaceAll("\r", "");
		pathStr = pathStr.replaceAll("\n", "");

		// フルパスをカンマ区切りで分割する。
		String[] pathList = pathStr.split(",");
		ArrayList<String> array = new ArrayList<String>();
		for (String path : pathList) {
			if(!array.contains(path)) {
				array.add(path);
			}
		}
		// 以下の処理はパスの数だけ繰り返す
		for (String path : array) {

			// 改行コード除去
			path.replaceAll("\n", "");

			// カンマ連続時のスキップ処理
			if (path.length() == 0) {
				continue;
			}

			RoleDomain targetRole = null;

			// キャッシュ内存在チェック
			if (userVariables.roleMap.get(path) != null) {

				targetRole = userVariables.roleMap.get(path);

			} else {
				// パス分割
				String[] pathArray = path.split("/");
				if (pathArray.length != 0) {

					// DB存在チェック
					RoleCriteria roleCriteria = new RoleCriteria();
					roleCriteria.setName(pathArray[pathArray.length - 1]);
					// パスからグループ取得の処理
					for (RoleDomain roleDomain : roleService.getList(roleCriteria)) {
						if (EntryUtil.getFullpathRoleName(roleDomain).equals(path)) {
							targetRole = roleDomain;
							// キャッシュに保持
							userVariables.roleMap.put(path, roleDomain);
							break;
						}
					}
					if (targetRole == null) {
						// グループがDBに存在しない場合null返却
						return null;
					}
				} else {
					// パスが不正
					return null;
				}
			}
			// 割当判定
			boolean isRegist = true;
			RoleDomain belongRole = new RoleDomain();
			for (RoleDomain role : belongRoleList) {
				if (role.getId() == targetRole.getId()) {
					isRegist = false;
					belongRole = role;
				}
			}
			if (isRegist) {
				// 割当対象
				registRoleList.add(targetRole);
			} else {
				// 更新不要
				belongRoleList.remove(belongRole);
			}
		}
		// 割当解除対象
		for (RoleDomain role : belongRoleList) {
			deleteRoleList.add(role);
		}
		return returnList;
	}

	/**
	 * ロール割当・割当解除の処理。<br>
	 * <br>
	 *
	 * @param user ユーザ
	 * @param roleList ロール割当・割当解除分類済List(getGroupMapメソッドのreturn値)
	 * @throws Exception
	 */
	private void updateBelongRole(UserDomain user, List<List<RoleDomain>> roleList) throws Exception {
		if (roleList == null) {
			return;
		}
		// 割当
		for (RoleDomain registRole : roleList.get(0)) {
			roleService.addUser(registRole, user);
		}
		// 割当解除
		for (RoleDomain deleteRole : roleList.get(1)) {
			roleService.removeUser(deleteRole, user);
		}
	}



	/* (非 Javadoc)
	 * @see jp.co.ctc_g.eim.admin.business.service.AdminUserService#searchUser(jp.co.ctc_g.eim.admin.business.domain.criteria.AdminUserCriteria)
	 */
	public List<UserDomain> getList(AdminUserCriteria adminUserCriteria) throws Exception {

		final String BEAN_ID_GROUP_SERVICE = "groupServiceForUserSearch";
		final String BEAN_ID_GROUP_SERVICE_EXCLUDE_CHILDREN = "groupServiceExcludeChildrenForUserSearch";
		List<Long> belongingUserIdList = null;


		//
		// グループ検索 (グループ名が指定されていた時のみ実行)
		//

		if (adminUserCriteria.getGroupName() != null && adminUserCriteria.getGroupName().length() > 0) {
			GroupService groupService = null;
			if (adminUserCriteria.getIncludingChildGroup() == false)
				groupService = (GroupService)ApplicationContextLoader.getApplicationContext().getBean(BEAN_ID_GROUP_SERVICE_EXCLUDE_CHILDREN);
			else
				groupService = (GroupService)ApplicationContextLoader.getApplicationContext().getBean(BEAN_ID_GROUP_SERVICE);

			GroupCriteria groupCriteria = new GroupCriteria();
			groupCriteria.setName(adminUserCriteria.getGroupName());

			List<GroupDomain> groupList = groupService.getList(groupCriteria);

			// 所属ユーザ取得
			BelongingUserExtractor belongingUserExtractor = new BelongingUserExtractor();
			belongingUserIdList = belongingUserExtractor.getIdList(groupList, adminUserCriteria.getIncludingChildGroup());

			// 検索結果が0件の時
			if(belongingUserIdList == null || belongingUserIdList.size() == 0)
			{
				return new ArrayList<UserDomain>();
			}
		}

		if (belongingUserIdList != null && belongingUserIdList.size() > 0)
		{
			adminUserCriteria.setIds(new MultipleCriteria<Long>(belongingUserIdList));
		}

		//
		// ユーザ検索
		//

		List<UserDomain> userList = userService.getList(adminUserCriteria);

		return userList;
	}

	/**
	 * ユーザ検索時のインナークラスです。<br>
	 * グループツリーの再帰処理を行います。
	 */
	private class BelongingUserExtractor {
		/**
		 * グループリスト
		 */
		private Set<Long> userIdSet = null;

		/**
		 * 所属ユーザのUser IDのリストを取得します。
		 * @param groupList 対象グループ
		 * @param includingChildGroup 下位のグループを含める場合true
		 * @return 所属するユーザのユーザIDのリスト
		 */
		public List<Long> getIdList(List<GroupDomain> groupList, Boolean includingChildGroup) throws Exception
		{
			userIdSet = new HashSet<Long>();

			for (int i = 0; i < groupList.size(); i ++)
			{
				GroupDomain group = (GroupDomain)groupList.get(i);

				// 所属ユーザ
				setUserId(group);

				if (includingChildGroup)
				{
					// 子グループの所属 (再帰処理)
					setUserIdReccurently(group);
				}
			}

			return new ArrayList<Long>(userIdSet);
		}

		/**
		 * 再帰的に所属ユーザのUser IDのリストをメンバ変数userIdSetに設定します。
		 * @param groupList 対象グループのリスト
		 */
		private void setUserIdReccurently(GroupDomain group) throws Exception
		{
			// Child Group
			List<GroupDomain> childGroupList = group.getChildList();

			for (int i = 0; i < childGroupList.size(); i++)
			{
				// Group
				GroupDomain childGroup = (GroupDomain)childGroupList.get(i);

				// 所属ユーザ
				setUserId(childGroup);

				// 再帰処理
				setUserIdReccurently(childGroup);
			}
		}

		/**
		 * 所属ユーザのUser IDのリストをメンバ変数userIdSetに設定します。
		 * @param group 対象グループ
		 */
		private void setUserId(GroupDomain group)
		{
			// 所属ユーザ
			List<UserDomain> userList = group.getUserList();

			for (int j = 0; j < userList.size(); j ++)
			{
				// User
				UserDomain user = (UserDomain)userList.get(j);
				// 追加
				userIdSet.add(user.getId());
			}
		}
	}

	/**
	 * 改行数を取得します。
	 * @return addNewLine 改行数
	 */
	public int getAddNewLine() {
		return addNewLine;
	}

	/**
	 * 改行数を設定します。
	 * @param addNewLine 改行数
	 */
	public void setAddNewLine(int addNewLine) {
		this.addNewLine = addNewLine;
	}

	/**
	 * 文字コードを取得します。
	 * @return charset 文字コード
	 */
	public String getCharset() {
		return charset;
	}

	/**
	 * 文字コードを取得します。
	 * @return charset 文字コード
	 */
	public void setCharset(String charset) {
		this.charset = charset;
	}

	/**
	 * 改行コードを取得します。
	 * @return 改行コード
	 */
	public String getNewline() {
		return newline;
	}

	/**
	 * 改行コードを設定します。
	 * @param 改行コード
	 */
	public void setNewline(String newline) {
		this.newline = newline;
	}

	/**
	 * ユーザサービスを取得します。
	 * @return ユーザサービス
	 */
	public UserService getUserService() {
		return userService;
	}

	/**
	 * ユーザサービスを設定します。
	 * @param ユーザサービス
	 */
	public void setUserService(UserService userService) {
		this.userService = userService;
	}

	/**
	 * グループサービスを取得します。
	 * @return グループサービス
	 */
	public GroupService getGroupService() {
		return groupService;
	}

	/**
	 * グループサービスを設定します。
	 * @param グループサービス
	 */
	public void setGroupService(GroupService groupService) {
		this.groupService = groupService;
	}

	/**
	 * ロールサービスを取得します。
	 * @return ロールサービス
	 */
	public RoleService getRoleService() {
		return roleService;
	}

	/**
	 * ロールサービスを設定します。
	 * @param ロールサービス
	 */
	public void setRoleService(RoleService roleService) {
		this.roleService = roleService;
	}

	/**
	 * 属性タイプサービスを取得します。
	 * @return 属性タイプサービス
	 */
	public AttributeTypeService getAttributeTypeService() {
		return attributeTypeService;
	}

	/**
	 * 属性タイプサービスを設定します。
	 * @param 属性タイプサービス
	 */
	public void setAttributeTypeService(AttributeTypeService attributeTypeService) {
		this.attributeTypeService = attributeTypeService;
	}

	/**
	 * オブジェクトタイプサービスを取得します。
	 * @return オブジェクトタイプサービス
	 */
	public ObjectTypeService getObjectTypeService() {
		return objectTypeService;
	}

	/**
	 * オブジェクトタイプサービスを設定します。
	 * @param オブジェクトタイプサービス
	 */
	public void setObjectTypeService(ObjectTypeService objectTypeService) {
		this.objectTypeService = objectTypeService;
	}

	/**
	 * システム管理のユーザ操作に関する機能DAOを取得します。
	 * @return adminUserDao
	 */
	public AdminUserDao getAdminUserDao() {
		return adminUserDao;
	}

	/**
	 * システム管理のユーザ操作に関する機能DAOを設定します。
	 * @param adminUserDao システム管理のユーザ操作に関する機能DAO
	 */
	public void setAdminUserDao(AdminUserDao adminUserDao) {
		this.adminUserDao = adminUserDao;
	}
}
