package jp.co.ctc_g.eim.app.document.common.aop.plugin.impl;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import jp.co.ctc_g.eim.framework2.business.domain.box.BoxFileCopyFromEIMFileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.box.BoxFolderDomain;
import jp.co.ctc_g.eim.framework2.business.service.box.BoxFolderService;
import jp.co.ctc_g.eim.framework2.common.aop.plugin.ParameterConverterPlugIn;

/**
 * パラメータ変換プラグインの実装クラス
 * <br>
 * 入力値としてBoxFolderDomainを受け取り、登録先のBoxフォルダパスを取得するプラグインクラスです。
 * <br>
 */
public class BoxFolderIdToBoxFilePathConverterImpl implements ParameterConverterPlugIn {

	/** Boxフォルダサービス */
	private BoxFolderService boxFolderService;

	/**
	 * パラメータ変換の処理。<br>
	 * <br>
	 * BoxフォルダIDを受け取り、登録先のBoxフォルダパスを返却します。
	 * <br>
	 * @param source BoxフォルダIDです。
	 * @return 登録先のBoxフォルダパス
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。
	 * @since Ver6.6
	 */
	public Object convert(Object source) throws Exception {

		String boxFolderId = null;
		if (source instanceof BoxFileCopyFromEIMFileDomain) {
			boxFolderId = ((BoxFileCopyFromEIMFileDomain) source).getParentBoxFolderId();
		}
		BoxFolderDomain boxFolder = boxFolderService.getById(boxFolderId);
		if (!Objects.isNull(boxFolder)) {
			List<BoxFolderDomain> pathList = boxFolder.getPathList();
			return "/" + pathList.stream().map(path -> path.getName()).collect(Collectors.joining("/"));
		}

		return "";
	}

	/**
	 * Boxフォルダサービスを取得します。
	 * @return Boxフォルダサービス
	 */
	public BoxFolderService getBoxFolderService() {
		return boxFolderService;
	}

	/**
	 * Boxフォルダサービスを設定します。
	 * @param boxFolderService Boxフォルダサービス
	 */
	public void setBoxFolderService(BoxFolderService boxFolderService) {
		this.boxFolderService = boxFolderService;
	}

}
