package com.ji.hadoop.p001;

import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;

// Hadoop 작업 두 번째 단계 : Combine
// 두 번째 단계는 알아서 처리됨!

// Hadoop 작업 세 번째 단계 : Reduce
// 	in : sleepy <1,1,1>
//	out : sleepy 3		>> 인자 뒤의 두자리

// 인자 앞의 두자리는 Mapper쪽 뒤의 두자리와 같아야 함.
public class WCReducer extends Reducer<Text, IntWritable, Text, IntWritable>{
	
	private static final IntWritable SUM = new IntWritable();
	
	// sleepy <1,1,1> 한 세트 만날 때마다 호출할 method
	@Override
	protected void reduce(Text arg0,	// map 단계에서 해준 key 값 : sleepy
			Iterable<IntWritable> arg1,	// list 비스무리한 : <1,1,1>
			Reducer<Text, IntWritable, Text, IntWritable>.Context arg2)	// 결과 처리 용도
					throws IOException, InterruptedException {
		
		int sum = 0;
		// <1,1,1> 합치는 작업 
		for (IntWritable i : arg1) {
			// sum은 int, i는 IntWritable 형태이다. > 형 변환 해줘야 함.
			//		> .get()
			sum += i.get();	// 1 + 1 + 1 = 3
			
		}
		SUM.set(sum);
		arg2.write(arg0, SUM);
		
	}

}
