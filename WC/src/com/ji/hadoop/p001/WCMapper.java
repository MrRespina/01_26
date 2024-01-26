package com.ji.hadoop.p001;

import java.io.IOException;
import java.util.StringTokenizer;

import org.apache.hadoop.io.IntWritable;	// int 형 처리
import org.apache.hadoop.io.Text;	// String 형 처리
import org.apache.hadoop.mapreduce.Mapper;

// Hadoop 작업의 첫 번째 단계 : Map

// 1단계 : 다운받은 소설책을 분석하라고 넣어줄 것 !
//		> key : 없고, value : String (hadoop에서는 String이 Text)
// 2단계 : 결과를 받아오기
//		> key : String, value : int
//		ex) I, 1
//			am, 1
//			sleepy, 1

public class WCMapper extends Mapper<Object, Text, Text, IntWritable>{

	// 결과처리를 위해 자료형을 맞추려면 메소드 밖으로 빼서 맞출 것
	// 메모리 절약하기 위해서 > singleton 처리
	
	private static final Text WORD = new Text();
	private static final IntWritable ONE = new IntWritable(1);
	
	// map을 override >> 한 문장마다 이 method가 호출 될 것.
	@Override
	protected void map(Object key, // data가 있는지 없는지 체크하기 위함. (중요도 낮음)
			Text value, // **중요**	가져오는 한 문장 그 자체를 의미함. (한 줄의 문장을 가져올 때 사용)
			Mapper<Object, Text, Text, IntWritable>.Context context) // 결과 처리를 위한 용도
			throws IOException, InterruptedException {

		// 기존에 사용하던 String 객체로 바꿔주는 작업
		String line = value.toString();
		
		// 한 문단을 문장 단위로 나누어줌.
		StringTokenizer st = new StringTokenizer(line," ");
		
		while(st.hasMoreElements()) { // 반복문 돌려서
			// 결과처리 .... (Hadoop으로 처리)
			WORD.set(st.nextToken());
			context.write(WORD, ONE);
			
		}
		
	}
	
}
