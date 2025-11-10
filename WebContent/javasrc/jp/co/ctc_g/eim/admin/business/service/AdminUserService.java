package jp.co.ctc_g.eim.admin.business.service;

import java.util.List;

import jp.co.ctc_g.eim.admin.business.domain.ExportDomain;
import jp.co.ctc_g.eim.admin.business.domain.criteria.AdminUserCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;

import org.apache.poi.ss.usermodel.Workbook;

/**
 * システム管理のユーザ操作に関する機能を提供するサービス
 *
 */
public interface AdminUserService {
	
	/**
	 *エクスポートファイルチェック<br>
	 *以下の条件でエラーを出力します。
	 * ・管理者権限がない
	 * ・文字コードが取得できない
	 * ・改行コードが取得できない
	 * ・ユーザ検索数が上限を超えている
	 * 
	 * @param userCode 検索ユーザID
	 * @param userName 検索ユーザ名
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	void checkExportInfo(ExportDomain exportDomain) throws Exception;
	
	
	/**
	 *検索条件を指定してユーザ情報を検索<br>
	 *検索結果をxlsx形式に出力して、Workbookを返却する<br>
	 * 
	 * @param exportDomain エクスポートドメイン<br>
	 * @return Workbook xlsx形式のワークブック<br>
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	Workbook exportUser(ExportDomain exportDomain) throws Exception;
	
	
	/**
	 *xlsxファイルを読み込み、ユーザ情報をインポート<br>
	 *入力項目のエラーチェックを行い、エラー内容(行番号：エラー内容)を返却する<br>
	 * 
	 * @param filePath インポートするxlsxファイルのパス<br>
	 * @return String xlsxファイルのエラー内容<br>
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	String importUser(String filePath) throws Exception;
	
	/**
	 * ユーザ検索を行います。
	 * @param adminUserCriteria 検索条件
	 * @return ユーザ一覧
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 */
	List<UserDomain> getList(AdminUserCriteria adminUserCriteria) throws Exception;
}
