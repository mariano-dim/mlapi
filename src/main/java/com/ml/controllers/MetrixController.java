package com.ml.controllers;

import com.ml.exceptions.EmptyResultException;
import com.ml.model.Metric;
import com.ml.services.metric.MetricService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Collection;


@CrossOrigin
@RestController
@RequestMapping("/api/clima")
public class MetrixController extends AbstractController<Metric> {
    
    @Autowired
    private MetricService metricService;


    @RequestMapping(method = RequestMethod.GET, value="/")
    public ResponseEntity<Collection<Metric>> getAll() {

        System.out.println("getAllMetrics");

        Collection<Metric> metrics = metricService.getAllMetrics();

        return super.collectionResult(metrics);

    }

    @RequestMapping(method = RequestMethod.GET, value = "/dia/{dia}")
    public ResponseEntity<Metric> getByDia(@Validated @PathVariable("dia") String dia) {

            System.out.println("getByDia");
        System.out.println("Dia: " + dia);

        Metric metric = metricService.getByDia(dia);

        if (null == metric) {
            throw new EmptyResultException(Metric.class);
        }
        return super.singleResult(metric);

    }
    


}


