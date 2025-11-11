package jp.co.ctc_g.eim.admin.common.aop.plugin.impl;

import jp.co.ctc_g.eim.framework2.common.aop.plugin.ParameterConverterPlugIn;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

import org.apache.poi.ss.usermodel.Workbook;

/**
 * 入力値としてWorkbookを受け取り、ファイル名を返却するクラスです。
 */
public class WorkbookToObjectConverterPlugInImpl implements ParameterConverterPlugIn {
	/**
	 * Workbookを受け取り、ファイル名を返却します。
	 * @param source Excel用Workbook
	 * @return ワークブックのシート名に拡張子(xlsx)を付加した文字列
	 */
	public Object convert(Object source) throws Exception {
		Workbook workbook = (Workbook)source;
		return workbook.getSheetName(0) + ConfigUtils.getByKey("EXPORT_FILE_TYPE");
	}
}
