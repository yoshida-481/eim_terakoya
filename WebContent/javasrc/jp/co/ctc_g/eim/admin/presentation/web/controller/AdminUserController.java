package jp.co.ctc_g.eim.admin.presentation.web.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import jp.co.ctc_g.eim.admin.business.domain.ExportDomain;
import jp.co.ctc_g.eim.admin.business.service.AdminUserService;
import jp.co.ctc_g.eim.admin.presentation.web.dto.ImportDTO;
import jp.co.ctc_g.eim.admin.presentation.web.form.ExportForm;
import jp.co.ctc_g.eim.app.form.presentation.controller.RestController;

/**
 * 管理ユーザコントロールクラス
 * Spring MVC のコントローラクラスです。
 * @since Ver6.8
 */
@Controller
@RequestMapping(value = "/rest/user")
public class AdminUserController extends RestController {

	/** 管理ユーザに関する操作を行うサービス */
	private AdminUserService adminUserService;

	/**
	 * エクスポートファイルチェック<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.AdminUserService#checkExportInfo(ExportDomain exportDomain) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.AdminUserService#checkExportInfo(ExportDomain exportDomain)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/checkExportInfo", method=RequestMethod.POST)
	@ResponseBody
	public void checkExportInfo(@RequestBody ExportForm exportForm) throws Exception {
		this.adminUserService.checkExportInfo(exportForm.convert(new ExportDomain()));
	}

	/**
	 * xlsxファイルを読み込み、ユーザ情報をインポート<br>
	 * 入力項目のエラーチェックを行い、エラー内容(行番号：エラー内容)を返却する<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.AdminUserService#importUser(String filePath) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.AdminUserService#importUser(String filePath)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/importUser", method=RequestMethod.POST)
	@ResponseBody
	public ImportDTO importUser(@RequestParam("filePath") String filePath) throws Exception {
		ImportDTO importDTO = new ImportDTO();
		importDTO.setMessage(this.adminUserService.importUser(filePath));
		return importDTO;
	}

	/**
	 * 管理ユーザに関する操作を行うサービスを取得します。
	 * @return コードに関する操作を行うサービス
	 */
	public AdminUserService getAdminUserService() {
		return this.adminUserService;
	}

	/**
	 * 管理ユーザに関する操作を行うサービスを設定します。
	 * @param adminUserService 管理ユーザに関する操作を行うサービス
	 */
	public void setAdminUserService(AdminUserService adminUserService) {
		this.adminUserService = adminUserService;
	}

}
