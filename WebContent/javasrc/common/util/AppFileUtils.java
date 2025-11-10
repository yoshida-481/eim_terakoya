package common.util;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.business.dao.FileDao;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.FileCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;



public class AppFileUtils {
	
	/**
	 * 原本ファイル情報のリストを返します。
	 * 
	 * @param sess セッション
	 * @param objectList ファイルが紐づくオブジェクトのリスト
	 * @return 原本ファイル情報のリスト
	 * @throws Exception
	 */
	public static List<FileDomain> getOriginalFileList(EIMSession sess, List<EIMObject> objectList) throws Exception {

		if (objectList == null || objectList.size() == 0) {
			return new ArrayList<FileDomain>();
		}

		FileDao fileDao = (FileDao)ApplicationContextLoader.getContext().getBean("fileDao2");

		// デフォルトフォーマット用のマップを用意する
		HashMap<Long, EIMFormat> defFormatMap = new HashMap<Long, EIMFormat>();

		FileCriteria fileCriteria = new FileCriteria();
		FileCriteria.ValueListCondition condition =
				new FileCriteria.ValueListCondition(FileCriteria.FileItemEnum.OBJECT_AND_FORMAT);
		for (Iterator<EIMObject> it = objectList.iterator(); it.hasNext();) {
			// Object
			EIMObject object = (EIMObject) it.next();

			EIMObjectType objectType = object.getType();
			EIMFormat originalFormat = defFormatMap.get((long)objectType.getId());
			if(originalFormat == null)
			{
				originalFormat = FileUtils.getDefaultFormat(sess, objectType);
				defFormatMap.put((long)objectType.getId(), originalFormat);
			}
			if(originalFormat == null) {
				continue;
			}
			condition.addQueryForObjectAndFormat((long)object.getId(), (long)originalFormat.getId());	// 原本ファイル取得
		}

		fileCriteria.setValueListCondition(condition);
		List<FileDomain> fileList = fileDao.getList(fileCriteria);

		return fileList;
	}

	/**
	 * 公開ファイル情報のリストを返します。
	 * 
	 * @param sess セッション
	 * @param objectList ファイルが紐づくオブジェクトのリスト
	 * @return 公開ファイル情報のリスト
	 * @throws Exception
	 */
	public static List<FileDomain> getPublicFileList(EIMSession sess, List<EIMObject> objectList) throws Exception {

		if (objectList == null || objectList.size() == 0) {
			return new ArrayList<FileDomain>();
		}

		FileDao fileDao = (FileDao)ApplicationContextLoader.getContext().getBean("fileDao2");

		EIMFormat publicFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
		
		FileCriteria fileCriteria = new FileCriteria();
		FileCriteria.ValueListCondition condition =
				new FileCriteria.ValueListCondition(FileCriteria.FileItemEnum.OBJECT_AND_FORMAT);
		for (Iterator<EIMObject> it = objectList.iterator(); it.hasNext();) {
			// Object
			EIMObject object = (EIMObject) it.next();

			condition.addQueryForObjectAndFormat((long)object.getId(), (long)publicFormat.getId());	// 公開ファイル取得
		}

		fileCriteria.setValueListCondition(condition);
		List<FileDomain> fileList = fileDao.getList(fileCriteria);

		return fileList;
	}

	/**
	 * 指定されたファイル情報から、オブジェクトIDとファイル情報のマップを返却します。
	 * @param fileList ファイルのリスト
	 * @return オブジェクトIDとファイル情報のマップ
	 */
	public static Map<Long, FileDomain> getObjectIdAndFileMap(List<FileDomain> fileList) {
		Map<Long, FileDomain> map = new HashMap<Long, FileDomain>();
		if (fileList == null || fileList.size() == 0) {
			return map;
		}
		
		
		return fileList.stream().collect(
					Collectors.toMap(d -> d.getObject().getId(), d -> d));
	}

}
