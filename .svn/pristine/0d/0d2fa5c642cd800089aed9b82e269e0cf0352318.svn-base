package eim.command.common;

import java.util.Comparator;

import eim.command.business.service.result.EIMCommandResultDocument;

/**
 * EIMCommandResultDocumentデータクラスを、パスで比較する。
 * パスが存在しない場合、IDで比較する。
 * @author kanno
 *
 */
@SuppressWarnings("unchecked")
public class EIMCommandResultDocumentComparetor implements Comparator{

	public int compare(Object objA, Object objB)
	{
		// パス
		String pathA = ((EIMCommandResultDocument)objA).getPath();
		String pathB = ((EIMCommandResultDocument)objB).getPath();
		// オブジェクトID
		String ObjectIdA = String.valueOf(((EIMCommandResultDocument)objA).getTarget().getId());
		String ObjectIdB = String.valueOf(((EIMCommandResultDocument)objB).getTarget().getId());

		if(pathA != null && pathB != null){
			return pathA.compareTo(pathB);
		}else if (pathA != null && pathB == null){
			return -1;
		}else if (pathA == null && pathB != null){
			return 1;
		}else {
			return ObjectIdA.compareTo(ObjectIdB);
		}

	}

}
