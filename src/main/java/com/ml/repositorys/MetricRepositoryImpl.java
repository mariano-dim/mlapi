package com.ml.repositorys;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;

public class MetricRepositoryImpl implements MetricRepositoryCustom {

	@Autowired
	MongoTemplate mongoTemplate;


}
