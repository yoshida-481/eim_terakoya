package jp.co.ctc_g.eim.app.document.presentation.controller;

import java.util.List;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import jp.co.ctc_g.eim.app.form.business.domain.FileObjectCreatorDomain;
import jp.co.ctc_g.eim.app.form.business.service.FileObjectCreatorService;
import jp.co.ctc_g.eim.app.form.presentation.controller.RestController;
import jp.co.ctc_g.eim.app.form.presentation.form.FileObjectCreatorForm;

/**
 * ファイルオブジェクト作成コントロールクラス
 * Spring MVC のコントローラクラスです。
 * @since Ver6.8
 */
@Controller
@RequestMapping(value = "/rest/documentFileObjectCreator")
public class DocumentFileObjectCreatorController extends RestController {

	/** WebDAV通信の為の付加的な機能を提供するサービス */
	private FileObjectCreatorService fileObjectCreatorService;

	/**
	 * WebDAV編集用にファイルオブジェクトを複製します。<br>
	 * 本メソッドは{@link jp.co.ctc_g.eim.app.form.business.service.FileObjectCreatorService#copy(FileObjectCreatorDomain fileObjectCreatorDomain) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.FileObjectCreatorService#copy(FileObjectCreatorDomain fileObjectCreatorDomain)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/copy", method=RequestMethod.POST)
	@ResponseBody
	public List<FileObjectCreatorDomain> copy(@RequestBody FileObjectCreatorForm dto) throws Exception {
		FileObjectCreatorDomain fileObjectCreator = dto.convert(new FileObjectCreatorDomain());

		return fileObjectCreatorService.copy(fileObjectCreator);
	}
	
	/**
	 * WebDAV通信の為の付加的な機能を提供するサービスを取得します。
	 * @return WebDAV通信の為の付加的な機能を提供するサービス
	 */
	public FileObjectCreatorService getFileObjectCreatorService() {
		return fileObjectCreatorService;
	}

	/**
	 * WebDAV通信の為の付加的な機能を提供するサービスを設定します。
	 * @param fileObjectCreatorService WebDAV通信の為の付加的な機能を提供するサービス
	 */
	public void setFileObjectCreatorService(FileObjectCreatorService fileObjectCreatorService) {
		this.fileObjectCreatorService = fileObjectCreatorService;
	}
	
}
