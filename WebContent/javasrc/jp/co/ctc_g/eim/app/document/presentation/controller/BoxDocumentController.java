package jp.co.ctc_g.eim.app.document.presentation.controller;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.stereotype.Controller;
import org.springframework.util.Assert;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

import eim.util.EIMConfig;
import jp.co.ctc_g.eim.app.document.business.service.BoxDocumentService;
import jp.co.ctc_g.eim.app.document.presentation.dto.BoxDocumentCreateDTO;
import jp.co.ctc_g.eim.app.document.presentation.dto.ConfirmBoxDocumentDTO;
import jp.co.ctc_g.eim.framework2.business.domain.box.BoxFileDomain;
import jp.co.ctc_g.eim.framework2.business.service.FormatService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.exception.EIMApplicationException;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.presentation.web.controller.RestController;

/**
 * Boxファイルコントロールクラス Spring MVC のコントローラクラスです。
 * @since Ver6.44
 */
@Controller
@RequestMapping(value = "/rest/box/documents")
public class BoxDocumentController extends RestController implements InitializingBean {

	/** Boxファイルサービス */
	private BoxDocumentService boxDocumentService;
	/** オブジェクトサービス */
	private ObjectService objectService;
	/** フォーマットサービス */
	private FormatService formatService;


	/**
	 * Webアプリケーションコンテキスト起動時に実行されます。
	 * プロパティのNullチェックをします。
	 *
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		Assert.notNull(boxDocumentService, "boxDocumentService must be specified.");
		Assert.notNull(objectService, "objectService must be specified.");
		Assert.notNull(formatService, "formatService must be specified.");
	}


	/**
	 * Boxファイルを一時格納フォルダにダウンロードします。
	 * @param boxId Boxファイル情報
	 * @param request HTTPサーブレットリクエスト
	 * @return オブジェクトID
	 * @throws Exception
	 *
	 */
	@PostMapping(value = "/contents/temp")
	@ResponseBody
	public Long getBoxFile(@RequestBody List<Map<String, Object>> files, HttpServletRequest request) throws Exception {

		//一時オブジェクト作成
		Long tmpObjId = boxDocumentService.createObject(files, request);

		File folder = new File(EIMConfig.get("TEMP") + tmpObjId);
		request.setAttribute("tmpObjId", tmpObjId);

		if (!folder.mkdirs()) {
		    throw new Exception();
		}

		for (Map<String, Object> file : files) {
			String fileId = (String) file.get("fileId");

			//boxのファイルIDからファイル情報を取得
			BoxFileDomain boxFile = boxDocumentService.getById(fileId);
			String fileName = boxFile.getName();

			//boxファイルを一時格納フォルダに保存
			String filePath = folder.getPath() + "/" + fileName;
			try (FileOutputStream outputStream = new FileOutputStream(filePath)) {
				boxDocumentService.download(fileId, outputStream);
			}catch (IOException e) {
				throw e;
            }
		}

		return tmpObjId ;

	}

	/**
	 * ファイルの登録可否をチェックします。
	 * @param filesInfo Boxファイル情報
	 * @param resquest HTTPサーブレットリクエスト
	 * @return ファイル登録可否結果
	 * @throws Exception
	 * @since
	 */
	@PostMapping(value = "/contents/confirm")
	@ResponseBody
	public List<ConfirmBoxDocumentDTO> comfirmBoxDocument(@RequestBody Map<String, Object> filesInfo,
			HttpServletRequest request) throws Exception {

		List<ConfirmBoxDocumentDTO> results = boxDocumentService.confirmBoxDocument(filesInfo, request);

		return results;

	}

	/**
	 * ファイルの登録をします。
	 * @param createFile Boxファイル情報
	 * @param request HTTPサーブレットリクエスト
	 * @return ファイル登録情報
	 * @throws Exception
	 * @since
	 */
	@PostMapping(value = "/contents/createDocument")
	@ResponseBody
	public  List<BoxDocumentCreateDTO> createDocument(@RequestBody Map<String, Object>createFile,HttpServletRequest request) throws Exception {

		List<BoxDocumentCreateDTO> results = boxDocumentService.createDocument(createFile, request);

		return results;

	}

	/**
	 * ファイル/フォルダを削除します。
	 * @param deleteFile Boxファイル情報
	 * @param resquest HTTPサーブレットリクエスト
	 * @return
	 * @throws Exception
	 * @since
	 */
	@PostMapping(value = "/contents/delete")
	@ResponseBody
	public String delete(@RequestBody Map<String, Object> deleteFile, HttpServletRequest request)
			throws Exception {
		String id = (String) deleteFile.get("id");
		String type = (String) deleteFile.get("type");
		String name = (String) deleteFile.get("name");
		String path = (String) deleteFile.get("path");
		boxDocumentService.delete(id,type,name,path);
		return id;
	}

	/**
	 * EIMApplicationExceptionのハンドラです。 スーパーメソッドに委譲します。
	 * @param request リクエスト
	 * @param response レスポンス
	 * @param ex エクセプション
	 * @return ModelAndView
	 * @since Ver6.44
	 */
	@ExceptionHandler(EIMApplicationException.class)
	public ModelAndView EIMApplicationExceptionHandler(HttpServletRequest request, HttpServletResponse response, EIMApplicationException ex) {
		if(isDownloadRequest(request) && !request.getRequestURI().contains("delete.mvc")){
				boxDocumentService.deleteTmpFolder(request);
			}
		return super.EIMApplicationExceptionHandler(request, response, ex);

	}

	/**
	 * EIMExceptionのハンドラです。 スーパーメソッドに委譲します。
	 *
	 * @param request リクエスト
	 * @param response レスポンス
	 * @param ex エクセプション
	 * @return ModelAndView
	 * @since Ver6.44
	 */
	@ExceptionHandler(EIMException.class)
	public ModelAndView EIMExceptionHandler(HttpServletRequest request, HttpServletResponse response, EIMException ex) {
		return super.EIMExceptionHandler(request, response, ex);

	}


	/**
	 * ダウンロードのリクエストかどうか返却します。
	 *
	 * @param request リクエスト
	 * @return ダウンロードのリクエストならtrue
	 */
	private boolean isDownloadRequest(HttpServletRequest request) {
		return request.getRequestURI().indexOf("/contents") != -1;
	}

	/**
	 * Boxファイルサービスを設定します。
	 *
	 * @param boxFileService Boxファイルサービス
	 */
	public void setBoxDocumentService(BoxDocumentService boxDocumentService) {
		this.boxDocumentService = boxDocumentService;
	}

	/**
	 * オブジェクトサービスを設定します。
	 *
	 * @param objectService オブジェクトサービス
	 * @since Ver6.44
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}

	/**
	 * フォーマットサービスを設定します。
	 *
	 * @param formatService フォーマットサービス s * @since Ver6.44
	 */
	public void setFormatService(FormatService formatService) {
		this.formatService = formatService;
	}

}
