package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.web.multipart.MultipartFile;

import addon.PublishCommandAddOnPDFConvert;
import common.util.AppConstant;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import jp.co.ctc_g.eim.app.document.business.service.PublicDocumentService;
import jp.co.ctc_g.eim.app.document.common.util.FileUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.FileAccessDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.DirectoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionIn;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.PsedoAttributeTypeEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.FileAccessService;
import jp.co.ctc_g.eim.framework2.business.service.FormatService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.WorkflowService;
import jp.co.ctc_g.eim.framework2.common.exception.EIMApplicationException;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;
import jp.co.ctc_g.eim.framework2.integration.util.internal.FileUtil;

/**
 * @see jp.co.ctc_g.eim.app.document.business.service.PublicDocumentService
 */
public class PublicDocumentServiceImpl implements PublicDocumentService {

	/** オブジェクトサービス */
	private ObjectService objectService = null;

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService = null;

	/** フォーマットサービス */
	private FormatService formatService = null;

	/** 属性タイプサービス */
	private AttributeTypeService attributeTypeService = null;

	/** ファイルアクセスサービス */
	private FileAccessService fileAccessService = null;

	/** ワークフローサービス */
	private WorkflowService workflowService = null;


	/**
	 * 公開PDFを作成します。
	 *
	 * @param objdomain オブジェクトドメイン
	 * @throws Exception
	 */
	@Override
	public void createAsync(ObjectDomain objdomain) throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObject object = ObjectUtils.getObjectById(sess, objdomain.getId());

		if(object == null){
			throw new EIMApplicationException("EIM.ERROR.LOGIC.NODOCUMENT");
		}

		PublishCommandAddOnPDFConvert addon = new PublishCommandAddOnPDFConvert();

		// PDF変換処理実行日時
		Date pdfConvExecDate = AppObjectUtil.getDateAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
		// オブジェクトのMDATE（原本ファイルの更新日時）
		Date modifyDate = object.getModifyDate();
		// PDF変換処理状態
		long faildFlag = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"), AppConstant.FLAG_OFF);

		// 公開PDFの変換対象ではない
		if(object.getStatus() == null || !addon.isNeedAsyncProcess(sess, object)){
			throw new EIMApplicationException("EIM.ERROR.LOGIC.PDF.NOT.TARGET",object.getName());
		}

		// 上位にワークフロー付きフォルダがある
		long higherFolderId = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), -1);
		if (higherFolderId != -1)
		{
			throw new EIMApplicationException("EIM.ERROR.LOGIC.EXIST.HIGHRANK.WFFOLDER", new Object[] {object.getName()});
		}

		// 原本の更新日時(MDATE)が「PDF変換処理実行日時」よりも小さい and PDF変換失敗状態でない
		if(pdfConvExecDate != null && modifyDate.getTime() < pdfConvExecDate.getTime() && faildFlag == AppConstant.FLAG_OFF){
			throw new EIMApplicationException("EIM.ERROR.LOGIC.PDF.CREATE",object.getName());
		}

		// ステータスが「公開処理中」または「公開済」
		if(object.getStatus().getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC || object.getStatus().getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC){
			throw new EIMApplicationException("EIM.ERROR.LOGIC.PDF.STATUS",object.getName());
		}

		// 公開PDFを作成
		addon.doAsyncProcess(sess, object);

		// アクセス履歴の登録
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.CREATE.PDF");

		// SearchFramework 検索FW更新通知 対象：ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CREATE_PUBLIC_PDF");
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PublicDocumentService#getPDFConversionProcessingObjectIdSet()
	 */
	public Set<Long> getPDFConversionProcessingObjectIdSet(List<Long>idList) throws Exception {

		if (idList.size() == 0) {
			// 対象なし
			return new HashSet<Long>();
		}

		// 検索条件設定
		SearchSelectObject searchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());
		searchSelectObject.setCondition(searchConditionGroup);

		// PDF変換のTYPE
		ObjectTypeDomain objectTypePDFConversion =
				objectTypeService.getByDefinitionName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_CONVPDF"));
		searchConditionGroup.addCondition(
				helper.eq(helper.opAnd(), PsedoAttributeTypeEnum.TYPE, objectTypePDFConversion.getId()));

		// PDF変換のNAME
		searchConditionGroup.addCondition(new SearchConditionIn(
				helper.opAnd(), PsedoAttributeTypeEnum.NAME, SearchOperatorEnum.IN,
				idList.stream().map(id -> String.valueOf(id)).toArray(String[]::new)));

		// 返却項目設定
		List<AttributeTypeDomain> resultAttributes = new ArrayList<AttributeTypeDomain>();
		resultAttributes.add(PsedoAttributeTypeEnum.NAME);
		searchSelectObject.setResultAttrs(resultAttributes);

		// 検索実行
		List<ObjectDomain> objectList = objectService.getList(searchSelectObject, null);

		return objectList.stream()
		        .map(ObjectDomain::getName)
		        .map(Long::valueOf)
		        .collect(Collectors.toSet());
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PublicDocumentService#preRegistPublicPdf()
	 */
	@Override
	public void preRegistPublicPdf(ObjectDomain object, MultipartFile file) throws Exception {
		// フォーマット存在チェック
		FormatDomain format = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PUBLIC"));
		if (format == null) {
			throw new EIMException("EIM.ERROR.LOGIC.FORMAT.NOTFOUND");
		}

		// PDF変換対象オブジェクトかどうか
		if (!isPdfConvertTarget(object.getId())) {
			throw new EIMException("EIM.ERROR.LOGIC.PDF.NOT.TARGET" ,object.getName());
		}

		// ファイルアップロード
		DirectoryDomain dir = format.getOnlineDirectory();
		long size = FileUtils.upload(dir.getPath() + object.getId() + ".pdf", file.getInputStream());

		// チェックイン
		FileUtils.checkin(object, format, file.getOriginalFilename(), size);

		// 属性更新
		// DBサーバーのシステム日時で「公開PDF事前登録日時」を設定する（objectService.setAttributeSingleDateは更新日が更新されてしまうため、util使用）
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_PRE_REGIST_DATE"));
		// 更新日時取得のためオブジェクト再取得
		object = objectService.getById(object.getId());
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(object.getModificationDate());
		// 公開PDF事前登録日時＞オブジェクトの更新日時　としたいので、+5秒する。
		calendar.add(Calendar.SECOND, 5);
		ObjectAttributeUtils.setAttribute(sess, ConvertUtils.toEIMObject(object), attType, calendar.getTime());

		// PDF変換処理実行日時を削除する
		AttributeTypeDomain attTypePdfConvExecDate = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
		objectService.removeAttribute(object, attTypePdfConvExecDate);

		// 属性「公開処理失敗」を削除する
		AttributeTypeDomain attTypePubProcFail = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"));
		objectService.removeAttribute(object, attTypePubProcFail);

	}

	/**
	 * PDF変換対象オブジェクトかどうかをワークフローから判定します。
	 *
	 * @param id オブジェクトID
	 * @throws Exception
	 */
	private boolean isPdfConvertTarget(long id) throws Exception {
		ObjectDomain object = objectService.getById(id);
		WorkflowDomain workflow = workflowService.getByStatusType(object.getStatus().getType());

		if (workflow == null) {
			return false;
		}

		List<StatusTypeDomain> statusTypeList = workflow.getStatusTypeList();
		ObjectTypeDomain pubObjType = objectTypeService.getByDefinitionName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_WFPUB"));
		long stTypeId = 0;
		for(StatusTypeDomain statusType : statusTypeList){
			if(statusType.getBase().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC){
				stTypeId = statusType.getId();
				break;
			}
		}
		long wfPubPdfConvertFlg = 0;
		if (stTypeId != 0) {
			ObjectDomain wfPubObj = objectService.getByTypeAndName(pubObjType, String.valueOf(stTypeId));
			wfPubPdfConvertFlg = wfPubObj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WFPUB_PDF_FLG")).getLong();
		}
		if (wfPubPdfConvertFlg == 0) {
			return false;
		}

		return true;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PublicDocumentService#deletePreRegistPdf()
	 */
	@Override
	public void deletePreRegistPdf(ObjectDomain object) throws Exception {

		FormatDomain publicFormat = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PUBLIC"));

		FileAccessDomain fileAccess = fileAccessService.getByObjectAndFormat(object, publicFormat);

		if (fileAccess == null) {
			return;
		}

		FileDomain file = fileAccess.getFile();
		fileAccess.getInputStream().close();

		// ファイル削除
		if (file != null) {
			new File(file.getDirectory().getPath() + FileUtil.getFileName(object, file)).delete();
		}
		fileAccessService.delete(fileAccess);

		// 公開PDF事前登録日時を削除する
		AttributeTypeDomain attTypePdfRegistDate = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_PRE_REGIST_DATE"));
		objectService.removeAttribute(object, attTypePdfRegistDate);

		// PDF変換処理実行日時を削除する
		AttributeTypeDomain attTypePdfConvExecDate = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
		objectService.removeAttribute(object, attTypePdfConvExecDate);

	}

	/**
	 * @param objectService セットします objectService
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}

	/**
	 * @return objectService
	 */
	public ObjectService getObjectService() {
		return objectService;
	}

	/**
	 * @return objectTypeService
	 */
	public ObjectTypeService getObjectTypeService() {
		return objectTypeService;
	}

	/**
	 * @param objectTypeService セットします objectTypeService
	 */
	public void setObjectTypeService(ObjectTypeService objectTypeService) {
		this.objectTypeService = objectTypeService;
	}

	/**
	 * @return formatService
	 */
	public FormatService getFormatService() {
		return formatService;
	}

	/**
	 * @param formatService セットする formatService
	 */
	public void setFormatService(FormatService formatService) {
		this.formatService = formatService;
	}

	/**
	 * @return attributeTypeService
	 */
	public AttributeTypeService getAttributeTypeService() {
		return attributeTypeService;
	}

	/**
	 * @param attributeTypeService セットする attributeTypeService
	 */
	public void setAttributeTypeService(AttributeTypeService attributeTypeService) {
		this.attributeTypeService = attributeTypeService;
	}

	/**
	 * @return fileAccessService
	 */
	public FileAccessService getFileAccessService() {
		return fileAccessService;
	}

	/**
	 * @param fileAccessService セットする fileAccessService
	 */
	public void setFileAccessService(FileAccessService fileAccessService) {
		this.fileAccessService = fileAccessService;
	}

	/**
	 * @return workflowService
	 */
	public WorkflowService getWorkflowService() {
		return workflowService;
	}

	/**
	 * @param workflowService セットする workflowService
	 */
	public void setWorkflowService(WorkflowService workflowService) {
		this.workflowService = workflowService;
	}
}
