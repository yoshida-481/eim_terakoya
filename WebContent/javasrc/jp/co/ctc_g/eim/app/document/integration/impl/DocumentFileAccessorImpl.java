package jp.co.ctc_g.eim.app.document.integration.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.lang3.StringUtils;

import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.integration.file.impl.FileAccessorImpl;
import jp.co.ctc_g.eim.framework2.integration.util.internal.FileUtil;

/**
 * 【ドキュメントAPI】
 * 実ファイル操作クラス
 */
public class DocumentFileAccessorImpl extends FileAccessorImpl {

	/** I/Oに必要なバッファサイズ */
	private int bufferdSize = 0;

	/** フラッシュする際のバッファサイズ */
	private int flushSize = 0;

	/** Zipファイル名の文字コード*/
	private String zipEncoding = "MS932";

	/** 定数（"."） */
	private static String DOT = ".";

	/** 定数（"_"） */
	private static String UNDER_BAR = "_";

	/**
	 * @see jp.co.ctc_g.eim.framework2.business.file.FileAccessor#create(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain, java.io.InputStream)
	 */
	@Override
	public FileDomain create(ObjectDomain object, FileDomain file, InputStream inFile) throws Exception {

		checkArgument(object, file);

		if (inFile == null) {
			// 引数inFileとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.INFILE.VALUE.ILLEGAL");
		}

		// 出力先ファイルの設定(入力ファイルのオンラインディレクトリに出力)
		String targetFilePath = getFilePath(object, file);
		File targetFile = new File(targetFilePath);
		if (targetFile.exists()) {
			// 既に同一名称のファイルが存在します。
			throw new EIMException("EIM.ERROR.LOGIC.FILE.EXISTS");
		}

		// 出力
		OutputStream out = new FileOutputStream(targetFile);
		long outSize = super.storeFile(inFile, out);

		// 返却値の設定
		FileDomain retFile = file.clone();
		retFile.setSize(outSize);

		return retFile;
	}

	/**
	 * @see jp.co.ctc_g.eim.framework2.business.file.FileAccessor#getStream(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain)
	 */
	@Override
	public InputStream getStream(ObjectDomain object, FileDomain file) throws Exception {

		checkArgument(object, file);

		InputStream in = null;
		try {
			FileUtil.prepareFileAccess(object, file);

			in = new FileInputStream(getFilePath(object, file));
		} catch(FileNotFoundException e) {
			return null;
		}

		return in;
	}

	/**
	 * 引数チェックを実施します。
	 * @param object チェック対象のオブジェクト情報
	 * @param file チェック対象のファイル情報
	 */
	private void checkArgument(ObjectDomain object, FileDomain file) throws Exception {

		if (object == null) {
			// 引数objectとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		if (file == null) {
			// 引数fileとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.FILE.VALUE.ILLEGAL");
		}

		if (file.getDirectory() == null) {
			// ディレクトリの指定がありません。
			throw new EIMException("EIM.ERROR.LOGIC.FILE.DIRECTORY.VALUE.ILLEGAL");
		}

		if (StringUtils.isBlank(file.getDirectory().getPath())) {
			// パスにnullまたは空文字が入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.DIRECTORY.PATH.LENGTH.ILLEGAL");
		}

		if (StringUtils.isBlank(file.getName())) {
			// ファイルの名称にnullまたは空文字が入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.FILE.NAME.LENGTH.ILLEGAL");
		}

	}

	/**
	 * 物理ファイルのパスを取得します。
	 *
	 * @param object オブジェクト
	 * @param file ファイル
	 * @return パス
	 */
	private String getFilePath(ObjectDomain object, FileDomain file) throws Exception {

		// 物理ファイルのディレクトリまでのパスと、オブジェクトID(物理ファイル名の一部)を取得
		String path = file.getDirectory().getPath() + object.getId();

		// シーケンスが設定されていた(framework Ver5.0以降でファイル追加・更新)場合
		if (file.getSequence() != 0) {

			// 物理ファイル名にシーケンスを付加
			path = path.concat( UNDER_BAR + String.valueOf(file.getSequence()) );
		}
		
		// 拡張子を取得
		String ext = file.getExt();

		// 拡張子が設定されていた場合
		if (ext != null) {

			// 拡張子を付加
			path = path.concat(ext);
		}
		
		//ファイルの存在確認
		File fileObject = new File(path);
		if(!fileObject.exists()){
			if (ext != null) {
				path = file.getDirectory().getPath() + object.getId() + ext;
			} else {
				path = file.getDirectory().getPath() + object.getId();
			}		
		}
		
		return path;
	}

}
