package jp.co.ctc_g.eim.app.document.presentation.controller;

import jakarta.servlet.http.HttpServletResponse;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;

import jp.co.ctc_g.eim.app.form.business.service.FormAttachedFileService;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessHistoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.AccessHistoryService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import jp.co.ctc_g.eim.framework2.presentation.web.controller.FileTransferController;

/**
 * ファイル転送コントロールクラス
 * Spring MVC のコントローラクラスです。
 */
@Controller
@RequestMapping(value = "/eim/app/document/file_io")
public class DocumentFileTransferController extends FileTransferController {

	/** オブジェクトサービス */
	private ObjectService objectService;
	/** アクセス履歴サービス */
	private AccessHistoryService accessHistoryService;
	/** 帳票添付ファイルサービス */
	private FormAttachedFileService formAttachedFileService;

	/** フォワード先JSP定義 */
	private final String ERROR_PAGE = "/servlet/HtmlErrorMessageServlet";
	/** エラーメッセージ定義 */
	private final String ERROR_ATTR_KEY = "errorMessage";

	@RequestMapping(value = "/retrieve_document_file")
	public ModelAndView getDocumentFile(@RequestParam("objectId") long objectId, HttpServletResponse response) throws Exception {

		try {

			// ファイルダウンロード
			super.get(objectId, response);

			// 添付元文書取得
			ObjectDomain document = formAttachedFileService.getParentByFileId(objectId);

			if(document != null) {
				// 添付ファイル取得
				ObjectDomain attachedFile = objectService.getById(objectId);

				// アクセス履歴(ダウンロード)登録
				AccessHistoryDomain accessHistory = new AccessHistoryDomain();
				accessHistory.setAction("EIM.ACCESS.TYPE.ATTATCHED.FILE.DOWNLOAD" + ConfigUtils.getByKey("ACCESS_HISTORY_DELIMITER") + attachedFile.getName());
				accessHistoryService.create(document, accessHistory);
			}
			return null;

		} catch(Exception e) {

			e.printStackTrace();

			ModelAndView modelAndView = new ModelAndView(ERROR_PAGE);
			modelAndView.addObject(ERROR_ATTR_KEY, ResourceUtils.getByKey("EIM.ERROR.LOGIC.NODOCUMENT"));
			return modelAndView;
		}
	}


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
	 * アクセス履歴サービスを取得します。
	 * @return アクセス履歴サービス
	 */
	public AccessHistoryService getAccessHistoryService() {
		return accessHistoryService;
	}

	/**
	 * アクセス履歴サービスを設定します。
	 * @param accessHistoryService アクセス履歴サービス
	 */
	public void setAccessHistoryService(AccessHistoryService accessHistoryService) {
		this.accessHistoryService = accessHistoryService;
	}

	/**
	 * 帳票添付ファイルサービスを取得します。
	 * @return 帳票添付ファイルサービス
	 */
	public FormAttachedFileService getFormAttachedFileService() {
		return formAttachedFileService;
	}

	/**
	 * 帳票添付ファイルサービスを設定します。
	 * @param formAttachedFileService 帳票添付ファイルサービス
	 */
	public void setFormAttachedFileService(FormAttachedFileService formAttachedFileService) {
		this.formAttachedFileService = formAttachedFileService;
	}
}
