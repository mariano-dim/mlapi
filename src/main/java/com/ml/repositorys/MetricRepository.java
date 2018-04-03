package com.ml.repositorys;

import com.ml.model.Metric;
import org.springframework.data.mongodb.repository.MongoRepository;

import org.springframework.data.mongodb.repository.Query;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

@RepositoryRestResource(collectionResourceRel = "metric", path = "metric")
public interface MetricRepository extends MongoRepository<Metric, String>, MetricRepositoryCustom {

	/**
	 *
	 * @param dia
	 * @return
	 */
	@Query("{ 'dia' : ?0}")
	public Metric findByDia(String dia);




}
