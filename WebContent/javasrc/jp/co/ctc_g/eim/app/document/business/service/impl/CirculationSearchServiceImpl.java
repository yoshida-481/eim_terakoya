package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import jakarta.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import common.util.AppLogicUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMStatus;
import eim.bo.EIMStatusType;
import eim.net.EIMSession;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.FileUtils;
import jp.co.ctc_g.eim.app.document.business.domain.CirculationSearchDomain;
import jp.co.ctc_g.eim.app.document.business.service.CirculationSearchService;
import jp.co.ctc_g.eim.app.document.business.service.PublicDocumentService;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.StatusCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.UserCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AssignmentDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionIn;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.PsedoAttributeTypeEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.StatusService;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;

/**
 * 回付状況一覧検索に関する操作を行うビジネスサービスです。
 * @see jp.co.ctc_g.eim.app.document.business.service.CirculationSearchService
 */
public class CirculationSearchServiceImpl implements CirculationSearchService {

	/** オブジェクトサービス */
	private ObjectService objectService;
	
	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService;
	
	/** ユーザサービス */
	private UserService userService;
	
	/** 属性タイプサービス */
	private AttributeTypeService attributeTypeService;
	
	/** ステータスサービス */
	private StatusService statusService;
	
	/** 承認中 (検索対象ドキュメントがWFの途中で未公開である場合) */
	private static final String TARGET_STATUS_REQUIERED = "0";
	
	/** 承認依頼中 (検索対象を承認依頼者に設定 */
	private static final String TARGET_USER_REQUIRED = "0";
	
	/** 承認待ち (検索対象を承認者に設定) */
	private static final String TARGET_USER_WAITING = "1";
	
	/** 承認済 (検索対象を過去の承認者に設定) */
	private static final String TARGET_USER_APPROVED = "2";
	
	/** SQL実行パラメータ用日付フォーマット */
	private static final SimpleDateFormat sqlParamFmt = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
	
	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.CirculationSearchService#search()
	 */
	public List<CirculationSearchDomain> search(HttpServletRequest request) throws Exception {
		
		// Session
		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();

		// ログイン言語により日付のフォーマットを変更
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
		String langId = sess.getLangId();
		if (AppConstant.LANG_VALUE_JA.equals(langId)) {
			sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");

		} else if (AppConstant.LANG_VALUE_EN.equals(langId)) {
			sdf = new SimpleDateFormat("MM-dd-yyyy HH:mm:ss");
		}

		// 「一般フォルダ」を親に持つフォルタイプを再帰的に取得
		ObjectTypeDomain folderObject = objectTypeService.getByDefinitionName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_FOLDER"));
		List<Long> folderTypeIds = new ArrayList<Long>();
		folderTypeIds = makeChildList(folderObject.getChildList(), folderTypeIds);

		// 「一般ドキュメント」を親に持つドキュメントタイプを再帰的に取得
		ObjectTypeDomain documentObject = objectTypeService.getByDefinitionName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_DOCUMENT"));
		List<Long> documentTypeIds = new ArrayList<Long>();
		documentTypeIds = makeChildList(documentObject.getChildList(), documentTypeIds);

		// 検索条件生成
		SearchSelectObject searchSelectObject = makeSearchCondition(request, folderTypeIds, documentTypeIds);

		// 取得件数上限設定(config.properties優先)
		int searchResultMax = EIMSearchLimitCountCondition.NOT_SPECIFIED;
		String searchResultMaxStr = ConfigUtils.getByKey("SEARCH.RESULT.MAX");
		if (searchResultMaxStr != null) {
			searchResultMax = Integer.parseInt(searchResultMaxStr);
		}
		SearchLimitCountCondition limitCond = new SearchLimitCountCondition(searchResultMax, true);

		//---------------------------------------------------------------------
		// 参照可能ドキュメントを取得
		//---------------------------------------------------------------------
		// 権限設定：参照権限あり
		searchSelectObject.setAccessRoleType(new AccessRoleTypeDomain(EIMAccessRole.READ));
		// 検索実行
		List<ObjectDomain> readableObjectList = objectService.getList(searchSelectObject, limitCond);

		//---------------------------------------------------------------------
		// 常時参照可能ドキュメントを取得し、IDをリスト化
		//---------------------------------------------------------------------
		// 権限設定：常時参照権限あり
		searchSelectObject.setAccessRoleType(new AccessRoleTypeDomain(AppConstant.ACCESS_ROLE_ALWAYS_READ));
		// 返却値設定(IDのみ)
		List<AttributeTypeDomain> resultAttrs = new ArrayList<AttributeTypeDomain>();
		resultAttrs.add(PsedoAttributeTypeEnum.ID);
		searchSelectObject.setResultAttrs(resultAttrs);
		// 検索実行
		List<ObjectDomain> alwaysReadableObjectList = objectService.getList(searchSelectObject, limitCond);
		List<Long> alwaysReadableObjectIdList = new ArrayList<Long>();
		for (ObjectDomain alwaysReadableObject : alwaysReadableObjectList) {
			alwaysReadableObjectIdList.add(alwaysReadableObject.getId());
		}

		// PDF変換されているかのチェック用フォーマット
		EIMFormat formatPDF = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));

		ApplicationContext applicationContext = ApplicationContextLoader.getApplicationContext();
		PublicDocumentService publicDocumentService = (PublicDocumentService)applicationContext.getBean("publicDocumentService");
		
		// PDF変換中オブジェクトIDのSetを用意する
		Set<Long> pdfConversionObjectIdSet = publicDocumentService.getPDFConversionProcessingObjectIdSet(
			readableObjectList.stream().map(ObjectDomain::getId).collect(Collectors.toList()));
		
		// 各オブジェクトから必要な情報を抽出
		List<CirculationSearchDomain> resultList = new ArrayList<CirculationSearchDomain>();
		Set<Long> statusUserIds = new HashSet<Long>();
		Map<Long, List<StatusDomain>> folderStatusMap = new HashMap<Long, List<StatusDomain>>();
		for (ObjectDomain obj : readableObjectList) {
			CirculationSearchDomain circulationDomain = new CirculationSearchDomain(obj);
			circulationDomain.setSdf(sdf);
			// オブジェクトタイプに依存する属性を付与
			if (folderTypeIds.contains(obj.getType().getId())) {
				circulationDomain.setObjTypeName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_FOLDER"));
				circulationDomain.setIsFolder("true");
				circulationDomain.setIsWFFolder("true");
				circulationDomain.setIsDocument("false");
				circulationDomain.setIsFullPath("false");
				folderStatusMap.put(obj.getId(), obj.getStatusList());
			} else {
				circulationDomain.setObjTypeName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_DOCUMENT"));
				circulationDomain.setIsDocument("true");
				circulationDomain.setIsFolder("false");
				circulationDomain.setIsWFFolder("false");
				circulationDomain.setIsOldVer(String.valueOf(!obj.isLatest()));
				circulationDomain.setRev(String.valueOf(obj.getRevision()));
				// PDFアイコンの表示判定
				String searchRangeStartDateStr = getSanitizedKeyword(EIMUtils.getParameter(request, "searchRangeStartDate"));
				EIMObject eimobj = new EIMObject(obj.getId(), null, searchRangeStartDateStr, 0, false, null, null, null, null, null, null, false, false, 
						new EIMStatus(circulationDomain.getStatusId(), new EIMStatusType(circulationDomain.getStatusTypeId(), null, null, 0, circulationDomain.getStatusTypeKind()))); 
				Boolean isDspPdfIcon = AppLogicUtil.isDspPdfIcon(sess, eimobj, formatPDF);
				circulationDomain.setIsDspPdfIcon(String.valueOf(isDspPdfIcon));
				// PDF変換ステータス出力
				int pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_NONE;
				AttributeDomain attributePubProcFail = obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"));
				long pdfFailFlg = attributePubProcFail != null ? attributePubProcFail.getLong() : 0;
				String statusTypeName = circulationDomain.getStatusTypeName();

				AttributeDomain attributePDFConvExecDate = obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
				Date pdfConvExecDate = attributePDFConvExecDate != null ? attributePDFConvExecDate.getDate() : null;
				AttributeDomain attributePdfPreRegistDate = obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_PRE_REGIST_DATE"));
				Date pdfPreRegistDate = attributePdfPreRegistDate != null ? attributePdfPreRegistDate.getDate() : null;
				if (pdfConversionObjectIdSet.contains((long)obj.getId())) {
					// PDF変換中
					pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESSING;

					if (circulationDomain.getStatusTypeKind() != AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
						// 公開処理中以外の場合は(PDF変換中)を出力
						statusTypeName += ResourceUtils.getByKey("EIM.LABEL.PDF.CONVERT.PROCESSING"); // (PDF変換中)
					}
				} else if (pdfFailFlg == AppConstant.FLAG_ON) {
					// PDF変換失敗
					if (pdfConvExecDate == null || pdfConvExecDate.getTime() > obj.getModificationDate().getTime()) {
						// チェックイン後に変換失敗した場合のみPDF変換失敗扱い（チェックインしてもPDF変換失敗フラグがONのままのため）
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_FAILURE;
	
						statusTypeName += ResourceUtils.getByKey("EIM.LABEL.PDF.CONVERT.FAILED"); // (PDF変換失敗)
					}
				} else if (isDspPdfIcon) {
					// PDF変換完了
					if (pdfConvExecDate != null && pdfConvExecDate.getTime() <= obj.getModificationDate().getTime()) {
						// PDF変換処理実行日時がオブジェクトのMDATE（原本ファイルの更新日時）以前
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_NOT_SAME_ORIGINAL;
					} else {
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_SAME_ORIGINAL;
					}
					// 公開PDF事前登録日時が存在する場合はその判定を優先する
					if (pdfPreRegistDate != null && pdfPreRegistDate.getTime() <= obj.getModificationDate().getTime()) {
						// 公開PDF事前登録日時がオブジェクトのMDATE（原本ファイルの更新日時）以前
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_NOT_SAME_ORIGINAL;
					} else if (pdfPreRegistDate != null && pdfPreRegistDate.getTime() > obj.getModificationDate().getTime()) {
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_SAME_ORIGINAL;
					}
				}
				// nullでなければ公開PDF事前登録済み
				String isPdfPreRegistered = pdfPreRegistDate != null ? "true" : "false";
				circulationDomain.setIsPdfPreRegistered(isPdfPreRegistered);

				circulationDomain.setPdfConversionStatus(pdfConversionStatus);
				circulationDomain.setStatusTypeName(statusTypeName);

			}
			// 読取専用フラグを設定
			// ※常時読取権限あり、オブジェクトに存在しない場合読取専用
			if (alwaysReadableObjectIdList.contains(obj.getId())) {
				circulationDomain.setIsReadOnly("false");
			} else {
				circulationDomain.setIsReadOnly("true");
			}

			// ステータスリストをID順にソート
			statusListSort(obj.getStatusList());

			for (int i = obj.getStatusList().size() - 1; i >= 0; i--) {
				StatusDomain status = obj.getStatusList().get(i);
				// 直近の「編集中」の直後のステータスを取得	
				if (status.getType() != null
					&& status.getType().getBase() != null
					&& status.getType().getBase().getId() == AppConstant.STATUS_TYPE_KIND_ID_EDITTING
					&& i < (obj.getStatusList().size() - 1)
				) {
					StatusDomain editNextStatus = obj.getStatusList().get(i+1); 
					long statusCreateUser = editNextStatus.getCreationUser().getId();
					statusUserIds.add(statusCreateUser);
					circulationDomain.setRequestUserId(statusCreateUser);
					circulationDomain.setRequestDate(editNextStatus.getCreationDate());
					break;
				}
			}
			// 有効期限を設定
			AttributeDomain expirationDate = obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_EFFECT_DATE"));
			if (expirationDate != null) {
				circulationDomain.setExpiration(String.valueOf(DateUtils.judgeExpirationDate(sess, expirationDate.getDate())));
			}
			// 自動採番番号を設定
			String numberStr = "";
			AttributeDomain numberAttr = obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_NUMBER"));
			if (numberAttr != null) {
				numberStr = numberAttr.getString();
			}
			circulationDomain.setNumber(numberStr);
			
			resultList.add(circulationDomain);
		}

		// 承認依頼者の名称を取得
		fetchUserName(resultList, statusUserIds);

		// 承認依頼日の降順でソートして返却
		return sortByApprovalRequestDateDesc(resultList);
	}

	/**
	 * 検索条件オブジェクトを作成します。
	 * @param request リクエスト
	 * @param folderTypeIds フォルダのオブジェクトタイプIDリスト
	 * @param documentTypeIds ドキュメントのオブジェクトタイプIDリスト
	 * @return 作成された検索条件オブジェクト
	 * @throws Exception
	 */
	private SearchSelectObject makeSearchCondition(HttpServletRequest request, List<Long> folderTypeIds, List<Long> documentTypeIds) throws Exception {

		SearchSelectObject searchSelectObject = new SearchSelectObject();

		// 検索条件設定
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());

		// 両条件のオブジェクトタイプをSQL用に変換
		List<Long> objectTypeIds = documentTypeIds;
		objectTypeIds.addAll(folderTypeIds);
		String searchTargetObjType = "";
		for (int j = 0; j < objectTypeIds.size(); j++) {
			searchTargetObjType += objectTypeIds.get(j);
			if ( j != objectTypeIds.size() - 1) {
				searchTargetObjType += " , ";
			}
		}

		// リクエスト情報取得
		String targetDocType = getSanitizedKeyword(EIMUtils.getParameter(request, "targetDocType"));
		String pathSpecify = getSanitizedKeyword(EIMUtils.getParameter(request, "pathCondition"));
		String targetPath = getSanitizedKeyword(EIMUtils.getParameter(request, "searchPath"));

		// 検索対象ドキュメント抽出用SQL格納
		StringBuffer searchCriteriaSQLStringBuffer = null;
		if (TARGET_USER_REQUIRED.equals(targetDocType)) {
			searchCriteriaSQLStringBuffer = makeSQLforRequest(request, searchTargetObjType);

		} else if (TARGET_USER_WAITING.equals(targetDocType)) {
			searchCriteriaSQLStringBuffer = makeSQLforWait(request, searchTargetObjType);

		} else if (TARGET_USER_APPROVED.equals(targetDocType)) {
			searchCriteriaSQLStringBuffer = makeSQLforApproved(request, searchTargetObjType);
		}

		// 検索条件設定
		searchConditionGroup.addCondition(new SearchConditionIn(
				helper.opAnd(), PsedoAttributeTypeEnum.ID, SearchOperatorEnum.IN, searchCriteriaSQLStringBuffer.toString()));
		
		// パスを検索条件設定
		if ("true".equals(pathSpecify)) {
			searchConditionGroup.addCondition(helper.like(
				helper.opAnd() 
				, attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS"))
				, helper.opLike()
				, targetPath + "*"
			));
		}
		searchSelectObject.setCondition(searchConditionGroup);
		return searchSelectObject;
	}

	/**
	 * 指定されたユーザが承認依頼を出したドキュメントを検索するSQLを作成します。
	 * @param request リクエスト
	 * @param searchTargetObjType 検索対象オブジェクトタイプ
	 * @return 作成されたSQL文
	 * @throws Exception
	 */
	private StringBuffer makeSQLforRequest(HttpServletRequest request, String searchTargetObjType) throws Exception {

		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();
		String searchRangeStartDate = getSanitizedKeyword(EIMUtils.getParameter(request, "searchRangeStartDate"));
		String searchRangeEndDate = getSanitizedKeyword(EIMUtils.getParameter(request, "searchRangeEndDate"));
		String objectName = getSanitizedKeyword(EIMUtils.getParameter(request, "objectName"));
		String isContainOldVersion = getSanitizedKeyword(EIMUtils.getParameter(request, "isContainOldVersion"));
		String status = getSanitizedKeyword(EIMUtils.getParameter(request, "status"));
		String userId = getSanitizedKeyword(EIMUtils.getParameter(request, "userId"));

		StringBuffer sb = new StringBuffer();

		sb.append("select obj.id ");
		sb.append("  from EIMOBJ obj ");
		sb.append("      ,EIMST current_st ");
		sb.append("      ,EIMST target_st ");
		sb.append("      ,EIMST prev_target_st ");
		sb.append("      ,EIMSTTYPE current_sttype ");
		sb.append("      ,EIMSTTYPE target_sttype ");
		sb.append("      ,EIMSTTYPE prev_target_sttype ");
		sb.append("      ,EIMEV ev ");
		sb.append(" where obj.type in ( "+ searchTargetObjType + " ) ");
		// 現在のステータスを指定
		if (TARGET_STATUS_REQUIERED.equals(status)) {
			sb.append("   and current_sttype.kind in ( " + AppConstant.STATUS_TYPE_KIND_ID_APPROVE +" , " + AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC + " ) ");
		} else {
			sb.append("   and current_sttype.kind = " + AppConstant.STATUS_TYPE_KIND_ID_PUBLIC );
		}
		sb.append("   and current_st.type = current_sttype.id ");
		sb.append("   and current_st.latest = 1 ");
		sb.append("   and obj.status = current_st.sid ");
		// 検索対象のステータスを指定
		sb.append("   and target_sttype.kind = " + AppConstant.STATUS_TYPE_KIND_ID_APPROVE );
		sb.append("   and target_st.type = target_sttype.id ");
		sb.append("   and obj.id = target_st.oid ");
		// 検索対象の一つ前のステータスを指定
		sb.append("   and prev_target_sttype.kind = " + AppConstant.STATUS_TYPE_KIND_ID_EDITTING );
		sb.append("   and prev_target_st.type = prev_target_sttype.id ");
		sb.append("   and prev_target_st.latest = 1 ");
		sb.append("   and obj.id = prev_target_st.oid ");
		// 検索対象のステータスとイベントを結合
		sb.append("   and ev.stid_t = target_st.sid ");
		sb.append("   and ev.stid_f = prev_target_st.sid ");
		sb.append("   and ev.cuser in ( " + userId + " ) ");
		if (!StringUtils.isEmpty(searchRangeStartDate)) {
			Date startDate = DateUtils.editExpirationDate(sess, eim.util.StringUtils.getDateFromString(searchRangeStartDate, ResourceUtils.getByKey("EIM.FORMAT.DATETIME")));
			sb.append("   and ev.cdate >= TO_DATE('" + sqlParamFmt.format(startDate)  + "', 'YYYY/MM/DD HH24:MI:SS' ) ");
		}
		if (!StringUtils.isEmpty(searchRangeEndDate)) {
			Date endDate = DateUtils.editExpirationDate(sess, eim.util.StringUtils.getDateFromString(searchRangeEndDate, ResourceUtils.getByKey("EIM.FORMAT.DATETIME")));
			sb.append("   and ev.cdate <= TO_DATE('" + sqlParamFmt.format(endDate)  + "', 'YYYY/MM/DD HH24:MI:SS' ) ");
		}
		// オブジェクトの抽出条件を指定
		if (!StringUtils.isEmpty(objectName)) {
			sb.append("   and obj.name like '%" + objectName + "%' ");
		}
		if (!"true".equals(isContainOldVersion)) {
			sb.append("   and obj.latest = 1 ");
		}
		return sb;
	}

	/**
	 * 指定されたユーザの承認待ち状態のドキュメントを検索するSQLを作成します
	 * @param request リクエスト
	 * @param searchTargetObjType 検索対象オブジェクトタイプ
	 * @return sb 作成されたSQL文
	 * @throws Exception
	 */
	private StringBuffer makeSQLforWait(HttpServletRequest request, String searchTargetObjType) throws Exception {

		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();
		String searchRangeStartDate = getSanitizedKeyword(EIMUtils.getParameter(request, "searchRangeStartDate"));
		String searchRangeEndDate = getSanitizedKeyword(EIMUtils.getParameter(request, "searchRangeEndDate"));
		String objectName = getSanitizedKeyword(EIMUtils.getParameter(request, "objectName"));
		String isContainOldVersion = getSanitizedKeyword(EIMUtils.getParameter(request, "isContainOldVersion"));
		String userId = getSanitizedKeyword(EIMUtils.getParameter(request, "userId"));
		
		StringBuffer sb = new StringBuffer();

		sb.append("select obj.id ");
		sb.append("  from EIMOBJ obj ");
		sb.append("      ,EIMST current_st ");
		sb.append("      ,EIMSTTYPE current_sttype ");
		sb.append("      ,EIMAS asign ");
		// 承認依頼日が条件に入っていた場合、イベントを抽出条件とするためテーブルを結合する
		if (!StringUtils.isEmpty(searchRangeStartDate) || !StringUtils.isEmpty(searchRangeEndDate)) {
			sb.append("      ,EIMST target_st ");
			sb.append("      ,EIMSTTYPE target_sttype ");
			sb.append("      ,EIMEV ev ");
		}
		sb.append(" where obj.type in ( "+ searchTargetObjType + ") ");
		sb.append("   and current_sttype.kind = " + AppConstant.STATUS_TYPE_KIND_ID_APPROVE );
		sb.append("   and current_st.type = current_sttype.id ");
		sb.append("   and current_st.latest = 1 ");
		sb.append("   and obj.id = current_st.oid ");
		sb.append("   and obj.status = current_st.sid ");
		sb.append("   and asign.stid = obj.status ");
		sb.append("   and asign.entry in ( " + userId + " ) ");
		// 承認依頼日が条件に入っていた場合、イベントを抽出条件とするためテーブルを結合する
		if (!StringUtils.isEmpty(searchRangeStartDate) || !StringUtils.isEmpty(searchRangeEndDate)) {
			sb.append("   and target_sttype.kind = " + AppConstant.STATUS_TYPE_KIND_ID_EDITTING );
			sb.append("   and target_st.type = target_sttype.id ");
			sb.append("   and ev.stid_f = target_st.sid ");
			sb.append("   and obj.id = target_st.oid ");

			if (!StringUtils.isEmpty(searchRangeStartDate)) {
				Date startDate = DateUtils.editExpirationDate(sess, eim.util.StringUtils.getDateFromString(searchRangeStartDate, ResourceUtils.getByKey("EIM.FORMAT.DATETIME")));
				sb.append("   and ev.cdate >= TO_DATE('" + sqlParamFmt.format(startDate)  + "', 'YYYY/MM/DD HH24:MI:SS' ) ");
			}
			if (!StringUtils.isEmpty(searchRangeEndDate)) {
				Date endDate = DateUtils.editExpirationDate(sess, eim.util.StringUtils.getDateFromString(searchRangeEndDate, ResourceUtils.getByKey("EIM.FORMAT.DATETIME")));
				sb.append("   and ev.cdate <= TO_DATE('" + sqlParamFmt.format(endDate)  + "', 'YYYY/MM/DD HH24:MI:SS' ) ");
			}
		}
		if (!StringUtils.isEmpty(objectName)) {
			sb.append("   and obj.name like '%" + objectName + "%' ");
		}
		if (!"true".equals(isContainOldVersion)) {
			sb.append("   and obj.latest = 1 ");
		}
		
		return sb;
	}


	/**
	 * 指定されたユーザが承認したドキュメントを検索するSQLを作成します
	 * @param request リクエスト
	 * @param searchTargetObjType 検索対象オブジェクトタイプ
	 * @return sb 作成されたSQL文
	 * @throws Exception
	 */
	private StringBuffer makeSQLforApproved(HttpServletRequest request, String searchTargetObjType) throws Exception {

		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();
		String searchRangeStartDate = getSanitizedKeyword(EIMUtils.getParameter(request, "searchRangeStartDate"));
		String searchRangeEndDate = getSanitizedKeyword(EIMUtils.getParameter(request, "searchRangeEndDate"));
		String objectName = getSanitizedKeyword(EIMUtils.getParameter(request, "objectName"));
		String isContainOldVersion = getSanitizedKeyword(EIMUtils.getParameter(request, "isContainOldVersion"));
		String status = getSanitizedKeyword(EIMUtils.getParameter(request, "status"));
		String userId = getSanitizedKeyword(EIMUtils.getParameter(request, "userId"));

		StringBuffer sb = new StringBuffer();

		sb.append("select obj.id ");
		sb.append("  from EIMOBJ obj ");
		sb.append("      ,EIMST current_st ");
		sb.append("      ,EIMST target_st ");
		sb.append("      ,EIMST prev_target_st ");
		sb.append("      ,EIMSTTYPE current_sttype ");
		sb.append("      ,EIMSTTYPE target_sttype ");
		sb.append("      ,EIMSTTYPE prev_target_sttype ");
		sb.append("      ,EIMEV ev ");
		sb.append("      ,EIMEVTYPE evtype ");
		sb.append(" where obj.type in ( "+ searchTargetObjType + ") ");
		// 現在のステータスを指定
		if (TARGET_STATUS_REQUIERED.equals(status)) {
			sb.append("   and current_sttype.kind in ( " + AppConstant.STATUS_TYPE_KIND_ID_APPROVE +" , " + AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC + " ) ");
		} else {
			sb.append("   and current_sttype.kind = " + AppConstant.STATUS_TYPE_KIND_ID_PUBLIC );
		}
		sb.append("   and current_st.type = current_sttype.id ");
		sb.append("   and current_st.latest = 1 ");
		sb.append("   and obj.status = current_st.sid ");
		// 検索対象のステータスを指定
		sb.append("   and target_sttype.kind in ( " + AppConstant.STATUS_TYPE_KIND_ID_APPROVE  + " , " + AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC  + " , " + AppConstant.STATUS_TYPE_KIND_ID_PUBLIC + ") " );
		sb.append("   and target_st.type = target_sttype.id ");
		if (!StringUtils.isEmpty(searchRangeStartDate)) {
			Date startDate = DateUtils.editExpirationDate(sess, eim.util.StringUtils.getDateFromString(searchRangeStartDate, ResourceUtils.getByKey("EIM.FORMAT.DATETIME")));
			sb.append("   and target_st.cdate >= TO_DATE('" + sqlParamFmt.format(startDate)  + "', 'YYYY/MM/DD HH24:MI:SS' ) ");
		}
		if (!StringUtils.isEmpty(searchRangeEndDate)) {
			Date endDate = DateUtils.editExpirationDate(sess, eim.util.StringUtils.getDateFromString(searchRangeEndDate, ResourceUtils.getByKey("EIM.FORMAT.DATETIME")));
			sb.append("   and target_st.cdate <= TO_DATE('" + sqlParamFmt.format(endDate)  + "', 'YYYY/MM/DD HH24:MI:SS' ) ");
		}
		sb.append("   and obj.id = target_st.oid ");
		// 検索対象の一つ前のステータスを指定
		sb.append("   and prev_target_sttype.kind = " + AppConstant.STATUS_TYPE_KIND_ID_APPROVE );
		sb.append("   and prev_target_st.type = prev_target_sttype.id ");
		sb.append("   and prev_target_st.latest = 1 ");
		sb.append("   and obj.id = prev_target_st.oid ");
		// 検索対象のステータスとイベントを結合
		sb.append("   and ev.cuser in ( " + userId + " ) ");
		sb.append("   and ev.stid_t = target_st.sid ");
		sb.append("   and ev.stid_f = prev_target_st.sid ");
		// 検索対象のイベントタイプを承認に指定
		sb.append("   and evtype.evtid = ev.evtid ");
		sb.append("   and evtype.bevtid = " + AppConstant.BASE_EVENT_TYPE_ID_APPROVAL );
		// オブジェクトの抽出条件を指定
		if (!StringUtils.isEmpty(objectName)) {
			sb.append("   and obj.name like '%" + objectName + "%' ");
		}
		if (!"true".equals(isContainOldVersion)) {
			sb.append("   and obj.latest = 1 ");
		}

		return sb;
	}
	
	/**
	 * 指定されたオブジェクトタイプの子を再帰的に取得します。
	 * @param objectType オブジェクトタイプ
	 * @return オブジェクトタイプリスト
	 */
	private List<Long> makeChildList(List<ObjectTypeDomain> objectTypeList, List<Long> typeIds) {

		for (ObjectTypeDomain objType : objectTypeList) {
			typeIds.add(objType.getId());

			makeChildList(objType.getChildList(), typeIds);
		}

		return typeIds;
	}

	/**
	 * 検索用にサニタイズしたキーワードを取得する
	 * @param keyword キーワード
	 * @return サニタイズしたキーワード
	 * @throws Exception
	 */
	private String getSanitizedKeyword(String keyword) throws Exception {

		if (StringUtils.isEmpty(keyword)) {
			return keyword;
		}
		// シングルクォートをダブルクォートに変換
		String retStr = eim.util.StringUtils.sqlSanitize(keyword);
		
		// エスケープ文字で終わっていたら、SQL発行時にエラーにならないように回避
		if (retStr.endsWith("\\")) {
			retStr = retStr + " ";
		}
		// 特殊文字があればエスケープ
		if(retStr.indexOf("}") >= 0) {
			retStr = retStr.replaceAll("}", "}}");
		}
		return retStr;
	}

	/**
	 * 承認依頼者、アサイン情報のIDからユーザ名称を取得します。
	 * @param circulationDomainList 検索結果リスト
	 * @param statusUserIds 承認依頼者ユーザID一覧
	 * @throws Exception 例外処理
	 */
	private void fetchUserName(List<CirculationSearchDomain> circulationDomainList, Set<Long> statusUserIds) throws Exception{
		Map<Long, String> statusUserMap = new HashMap<Long, String>();
		UserCriteria userCriteria = new UserCriteria();
		MultipleCriteria<Long> multipleCriteria = new MultipleCriteria<Long>();
		multipleCriteria.addAll(statusUserIds);
		userCriteria.setIds(multipleCriteria);

		// 承認依頼ユーザID一覧を取得し、IDと名称のマッピングを行う
		List<UserDomain> statusUserList = userService.getList(userCriteria);
		for (UserDomain appUser : statusUserList) {
			String name = appUser.getName() == null ? appUser.getDefinitionName() : appUser.getName();
			statusUserMap.put(appUser.getId(), name);
		}

		// ステータスIDをリスト化
		MultipleCriteria<Long> statusIds = new MultipleCriteria<Long>(); 
		for (CirculationSearchDomain circulationDomain : circulationDomainList) {
			statusIds.add(circulationDomain.getStatusId());
		}
		// ステータスリストを取得
		StatusCriteria statusCriteria = new StatusCriteria();
		statusCriteria.setIds(statusIds);
		List<StatusDomain> statusDomainList = statusService.getList(statusCriteria);

		// ステータス／アサイン情報をMap化<key: ステータスID, value: アサイン先リスト>
		Map<Long, List<AssignmentDomain>> statusAssignMap = new HashMap<Long, List<AssignmentDomain>>();
		for (StatusDomain statusDomain : statusDomainList) {
			statusAssignMap.put(statusDomain.getId(), statusDomain.getAssignmentList());
		}
		// 承認依頼者、承認者情報をセット
		for (CirculationSearchDomain circulationDomain : circulationDomainList) {
			List<AssignmentDomain> assignmentList = statusAssignMap.get(circulationDomain.getStatusId());
			
			// 多言語名称が存在しない場合は定義名称をセットする
			for (AssignmentDomain assignment : assignmentList) {
				if (assignment.getEntryElement().getName() == null
						&& assignment.getEntryElement() instanceof UserDomain) {
					OtherNameDomain otherName = new OtherNameDomain();
					otherName.setLangId(jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession().getLangId());
					UserDomain user = (UserDomain)assignment.getEntryElement();
					otherName.setName(user.getDefinitionName());
					assignment.getEntryElement().getNameList().add(otherName);
				}
			}
			// アサインユーザを名称の昇順で並び替える
			assignmentListSort(assignmentList);

			// アサインユーザをパイプでつなげる
			String assignmentUserName = "";
			for (int i = 0; i < assignmentList.size(); i++) {
				if (i > 0) {
					assignmentUserName += "|";
				}
				assignmentUserName += assignmentList.get(i).getEntryElement().getName();
			}
			// 出力用ドメインにセット
			circulationDomain.setNextApprover(assignmentUserName);
			circulationDomain.setRequestUser(statusUserMap.get(circulationDomain.getRequestUserId()));
		}
	}
	
	/**
	 * ステータスリストをIDの昇順でソートします。
	 * @param statusList ステータスリスト
	 * @return ソート後リスト
	 */
	private List<StatusDomain> statusListSort(List<StatusDomain> statusList) {
		Collections.sort(statusList, new Comparator<StatusDomain>() {
			public int compare(StatusDomain obj1, StatusDomain obj2) {
				Long val1 = obj1.getId();
				Long val2 = obj2.getId();
				return val1.compareTo(val2);
			}
		});
		return statusList;
	}
	
	/**
	 * 承認者リストを名称の昇順でソートします。
	 * @param assignmentList 承認者リスト
	 * @return ソート後リスト
	 */
	private List<AssignmentDomain> assignmentListSort(List<AssignmentDomain> assignmentList) {
		Collections.sort(assignmentList, new Comparator<AssignmentDomain>() {
			public int compare(AssignmentDomain obj1, AssignmentDomain obj2) {
				String val1 = obj1.getEntryElement().getName();
				String val2 = obj2.getEntryElement().getName();
				return val1.compareTo(val2);
			}
		});
		return assignmentList;
	}
	
	/**
	 * 承認依頼日の降順でソートします。
	 * @param resultList 検索結果リスト
	 * @return 検索結果リスト
	 */
	private List<CirculationSearchDomain> sortByApprovalRequestDateDesc(List<CirculationSearchDomain> resultList) {
		Collections.sort(resultList, new Comparator<CirculationSearchDomain>() {
			public int compare(CirculationSearchDomain obj1, CirculationSearchDomain obj2) {
				Date val1 = obj1.getRequestDate();
				Date val2 = obj2.getRequestDate();
				return val2.compareTo(val1);
			}
		});
		return resultList;
	}
	
	//==================================
	// Getter/Setter
	//==================================
	/**
	 * オブジェクトサービスを取得します。
	 * @return オブジェクトサービス
	 */
	public ObjectService getObjectService() {
		return objectService;
	}

	/**
	 * オブジェクトサービスを設定します。
	 * @param objectService オブジェクトサービス
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
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
	 * @param objectTypeService オブジェクトタイプサービス
	 */
	public void setObjectTypeService(ObjectTypeService objectTypeService) {
		this.objectTypeService = objectTypeService;
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
	 * @param userService ユーザサービス
	 */
	public void setUserService(UserService userService) {
		this.userService = userService;
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
	 * @param attributeTypeService 属性タイプサービス
	 */
	public void setAttributeTypeService(AttributeTypeService attributeTypeService) {
		this.attributeTypeService = attributeTypeService;
	}

	/**
	 * ステータスサービスを取得します。
	 * @return ステータスサービス
	 */
	public StatusService getStatusService() {
		return statusService;
	}

	/**
	 * ステータスサービスを設定します。
	 * @param statusService ステータスサービス
	 */
	public void setStatusService(StatusService statusService) {
		this.statusService = statusService;
	}
}
